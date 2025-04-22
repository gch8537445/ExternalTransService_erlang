%%%-------------------------------------------------------------------
%%% @doc
%%% 曹操专车提供商
%%% 负责与曹操专车API通信，获取预估价等信息
%%% @end
%%%-------------------------------------------------------------------
-module(caocao_provider).
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

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 启动服务器
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server回调函数
%%====================================================================

%% @doc 初始化服务器
init([]) ->
    % 初始化状态
    State = #{},
    % 立即加载配置
    {ok, handle_info(refresh_config, State)}.

%% @doc 处理同步调用
handle_call({estimate_price, Params}, _From, State) ->
    % 提取参数
    Start = maps:get(start_location, Params),
    End = maps:get(end_location, Params),
    _UserId = maps:get(user_id, Params),

    % 获取配置
    Domain = maps:get(<<"CAOCAO_DOMAIN">>, State),
    ClientId = maps:get(<<"CAOCAO_CLIENT_ID">>, State),
    SignKey = maps:get(<<"CAOCAO_SIGN_KEY">>, State),

    % 构建请求参数
    [StartLat, StartLng] = binary:split(Start, <<",">>),
    [EndLat, EndLng] = binary:split(End, <<",">>),

    % 构建请求体
    RequestParams = #{
        <<"client_id">> => ClientId,
        <<"timestamp">> => integer_to_binary(erlang:system_time(second)),
        <<"start_lat">> => StartLat,
        <<"start_lng">> => StartLng,
        <<"end_lat">> => EndLat,
        <<"end_lng">> => EndLng
    },

    % 计算签名
    Sign = calculate_sign(RequestParams, SignKey),
    RequestParamsWithSign = RequestParams#{<<"sign">> => Sign},

    % 发送请求
    Url = <<Domain/binary, "/v2/common/estimatePriceWithDetail">>,
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
    ProviderKey = <<"provider:caocao">>,
    case redis_client:get(ProviderKey) of
        {ok, ConfigJson} ->
            try
                % 解析JSON配置
                NewState = jsx:decode(ConfigJson, [return_maps]),
                % 更新状态
                {noreply, NewState}
            catch
                Type:Reason:Stack ->
                    % 记录错误日志
                    logger:error(
                        "解析曹操配置失败: ~p:~p~n~p",
                        [Type, Reason, Stack]
                    ),
                    % 设置定时器稍后重试
                    erlang:send_after(5000, self(), refresh_config)
            end;
        {error, not_found} ->
            % 记录错误日志
            logger:error("未找到曹操的配置: ~p", [ProviderKey]);
        {error, Reason} ->
            % 记录错误日志
            logger:error("获取曹操配置失败: ~p", [Reason])
    end,
    {noreply, State};

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

%% @doc 计算签名
%% 按照曹操专车API的签名规则计算签名
calculate_sign(Params, SignKey) ->
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

    % 添加签名密钥
    SignStrWithKey = <<SignStr/binary, "key=", SignKey/binary>>,

    % 计算MD5
    crypto:hash(md5, SignStrWithKey).

%% @doc 提取车型和价格信息
extract_car_types(Data) ->
    % 提取车型列表
    CarList = maps:get(<<"car_types">>, Data, []),

    % 转换为标准格式
    lists:map(
        fun(Car) ->
            % 提取车型信息
            CarId = maps:get(<<"car_id">>, Car, <<>>),
            CarName = maps:get(<<"car_name">>, Car, <<>>),
            Price = maps:get(<<"price">>, Car, 0),

            % 构建标准格式
            #{
                provider => <<"caocao">>,
                car_id => CarId,
                car_name => CarName,
                price => Price,
                currency => <<"CNY">>,
                details => #{
                    base_price => maps:get(<<"base_price">>, Car, 0),
                    distance_price => maps:get(<<"distance_price">>, Car, 0),
                    time_price => maps:get(<<"time_price">>, Car, 0),
                    night_fee => maps:get(<<"night_fee">>, Car, 0)
                }
            }
        end,
        CarList
    ).
