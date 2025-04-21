%%%-------------------------------------------------------------------
%%% @doc
%%% T3出行提供商
%%% 负责与T3出行API通信，获取预估价等信息
%%% @end
%%%-------------------------------------------------------------------
-module(t3_provider).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server回调函数
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {
    config = #{} :: map()  % 提供商配置
}).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 启动服务器
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server回调函数
%%====================================================================

%% @doc 初始化服务器
init([]) ->
    % 注册到提供商管理器
    gen_server:cast(provider_manager, {register_provider, ?MODULE, self()}),

    % 初始化状态
    State = #state{},

    % 启动定时器，定期刷新配置
    {ok, _} = timer:send_interval(60000, refresh_config),

    % 立即加载配置
    {ok, handle_info(refresh_config, State)}.

%% @doc 处理同步调用
handle_call({estimate_price, Params}, _From, State) ->
    % 提取参数
    Start = maps:get(start_location, Params),
    End = maps:get(end_location, Params),
    _UserId = maps:get(user_id, Params),

    % 获取配置
    Config = State#state.config,
    Domain = maps:get(<<"T3_DOMAIN">>, Config, <<"https://api.t3go.cn">>),
    AppKey = maps:get(<<"T3_APP_KEY">>, Config, <<"">>),
    AppSecret = maps:get(<<"T3_APP_SECRET">>, Config, <<"">>),

    % 构建请求参数
    [StartLat, StartLng] = binary:split(Start, <<",">>),
    [EndLat, EndLng] = binary:split(End, <<",">>),

    % 构建请求体
    Timestamp = integer_to_binary(erlang:system_time(second)),
    Nonce = generate_nonce(),

    RequestParams = #{
        <<"app_key">> => AppKey,
        <<"timestamp">> => Timestamp,
        <<"nonce">> => Nonce,
        <<"start_lat">> => StartLat,
        <<"start_lng">> => StartLng,
        <<"end_lat">> => EndLat,
        <<"end_lng">> => EndLng
    },

    % 计算签名
    Sign = calculate_sign(RequestParams, AppSecret),
    RequestParamsWithSign = RequestParams#{<<"sign">> => Sign},

    % 发送请求
    Url = <<Domain/binary, "/open/price/estimate">>,
    case http_client:post(Url, RequestParamsWithSign) of
        {ok, 200, ResponseBody} ->
            % 解析响应
            try jsx:decode(ResponseBody, [return_maps]) of
                #{<<"code">> := 0, <<"data">> := Data} ->
                    % 提取车型和价格信息
                    CarTypes = extract_car_types(Data),
                    {reply, {ok, CarTypes}, State};
                #{<<"code">> := Code, <<"message">> := Message} ->
                    {reply, {error, {api_error, Code, Message}}, State}
            catch
                _:_ ->
                    {reply, {error, invalid_response}, State}
            end;
        {ok, StatusCode, _} ->
            {reply, {error, {http_error, StatusCode}}, State};
        {error, Reason} ->
            {reply, {error, {request_failed, Reason}}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @doc 处理异步调用
handle_cast(_Request, State) ->
    {noreply, State}.

%% @doc 处理信息
handle_info(refresh_config, State) ->
    % 从Redis直接加载配置
    ProviderKey = <<"provider:t3">>,
    case redis_client:get(ProviderKey) of
        {ok, ConfigJson} ->
            try
                % 解析JSON配置
                Config = jsx:decode(ConfigJson, [return_maps]),
                % 更新状态
                NewState = State#state{config = Config},
                % 注册配置到提供商管理器
                provider_manager:register_provider_config(?MODULE, Config),
                {noreply, NewState}
            catch
                Type:Reason:Stack ->
                    % 记录错误日志
                    logger:error(
                        "Failed to parse T3 provider config: ~p:~p~n~p", 
                        [Type, Reason, Stack]
                    ),
                    % 设置定时器稍后重试
                    erlang:send_after(5000, self(), refresh_config),
                    % 如果是初始化阶段（没有配置），则终止进程
                    case maps:size(State#state.config) of
                        0 -> {stop, {config_error, Reason}, State};
                        _ -> {noreply, State} % 保持现有配置继续运行
                    end
            end;
        {error, not_found} ->
            % 记录错误日志
            logger:error("T3 provider config not found: ~p", [ProviderKey]),
            % 设置定时器稍后重试
            erlang:send_after(5000, self(), refresh_config),
            % 如果是初始化阶段（没有配置），则终止进程
            case maps:size(State#state.config) of
                0 -> {stop, {config_error, not_found}, State};
                _ -> {noreply, State} % 保持现有配置继续运行
            end;
        {error, Reason} ->
            % 记录错误日志
            logger:error("Failed to get T3 provider config: ~p", [Reason]),
            % 设置定时器稍后重试
            erlang:send_after(5000, self(), refresh_config),
            % 如果是初始化阶段（没有配置），则终止进程
            case maps:size(State#state.config) of
                0 -> {stop, {config_error, Reason}, State};
                _ -> {noreply, State} % 保持现有配置继续运行
            end
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc 终止服务器
terminate(_Reason, _State) ->
    % 从提供商管理器注销
    gen_server:cast(provider_manager, {unregister_provider, ?MODULE}),
    ok.

%% @doc 代码更新
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc 生成随机字符串
%% 生成一个随机的nonce字符串
-spec generate_nonce() -> binary().
generate_nonce() ->
    % 生成16字节的随机数据
    RandomBytes = crypto:strong_rand_bytes(16),
    % 转换为十六进制字符串
    list_to_binary(lists:flatten([io_lib:format("~2.16.0b", [X]) || X <- binary_to_list(RandomBytes)])).

%% @doc 计算签名
%% 按照T3出行API的签名规则计算签名
-spec calculate_sign(map(), binary()) -> binary().
calculate_sign(Params, AppSecret) ->
    % 按照键名排序
    SortedKeys = lists:sort(maps:keys(Params)),

    % 构建签名字符串
    SignStr = lists:foldl(
        fun(Key, Acc) ->
            Value = maps:get(Key, Params),
            <<Acc/binary, Key/binary, "=", Value/binary, "&">>
        end,
        <<>>,
        SortedKeys
    ),

    % 添加应用密钥
    SignStrWithSecret = <<SignStr/binary, "secret=", AppSecret/binary>>,

    % 计算SHA256
    crypto:hash(sha256, SignStrWithSecret).

%% @doc 提取车型和价格信息
-spec extract_car_types(map()) -> [map()].
extract_car_types(Data) ->
    % 提取车型列表
    CarList = maps:get(<<"car_types">>, Data, []),

    % 转换为标准格式
    lists:map(
        fun(Car) ->
            % 提取车型信息
            CarTypeId = maps:get(<<"car_type_id">>, Car, <<>>),
            CarTypeName = maps:get(<<"car_type_name">>, Car, <<>>),
            EstimatePrice = maps:get(<<"estimate_price">>, Car, 0),

            % 提取价格详情
            PriceDetail = maps:get(<<"price_detail">>, Car, #{}),

            % 构建标准格式
            #{
                provider => <<"t3">>,
                car_id => CarTypeId,
                car_name => CarTypeName,
                price => EstimatePrice,
                currency => <<"CNY">>,
                details => #{
                    base_price => maps:get(<<"base_price">>, PriceDetail, 0),
                    distance_price => maps:get(<<"distance_price">>, PriceDetail, 0),
                    time_price => maps:get(<<"time_price">>, PriceDetail, 0),
                    night_fee => maps:get(<<"night_fee">>, PriceDetail, 0)
                }
            }
        end,
        CarList
    ).
