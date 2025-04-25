%%%-------------------------------------------------------------------
%%% @doc
%%% 曹操专车提供商
%%% 负责与曹操专车API通信，获取预估价等信息
%%% @end
%%%-------------------------------------------------------------------
-module(caocao_provider).

%% 导出函数
-export([
    init/1,
    estimate_price/1
]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 初始化
init([]) ->
    % 初始化状态
    State = #{},
    % 立即刷新配置
    refresh_config(),
    {ok, State}.

%% @doc 估算价格
estimate_price(Params) ->
    % 获取配置
    {ok, Config} = get_config(),
    
    % 提取参数
    Start = maps:get(start_location, Params),
    End = maps:get(end_location, Params),
    _UserId = maps:get(user_id, Params),

    % 获取配置
    Domain = maps:get(<<"CAOCAO_DOMAIN">>, Config),
    ClientId = maps:get(<<"CAOCAO_CLIENT_ID">>, Config),
    SignKey = maps:get(<<"CAOCAO_SIGN_KEY">>, Config),

    % 构建请求参数
    [StartLat, StartLng] = binary:split(Start, <<",">>),
    [EndLat, EndLng] = binary:split(End, <<",">>),

    % 构建请求体
    RequestParams = #{
        <<"client_id">> => ClientId,
        <<"timestamp">> => integer_to_binary(erlang:system_time(millisecond)),
        <<"from_latitude">> => StartLat,
        <<"from_longitude">> => StartLng,
        <<"to_latitude">> => EndLat,
        <<"to_longitude">> => EndLng,
        <<"car_type">> => <<"2,3,5,7,14,15">>,  % 支持多种车型
        <<"city_code">> => maps:get(<<"city_code">>, Config, <<"010">>),  % 默认北京
        <<"order_type">> => <<"1">>  % 实时单
    },

    % 计算签名
    Sign = calculate_sign(RequestParams,#{<<"sign_key">> => SignKey}),
    RequestParamsWithSign = RequestParams#{<<"sign">> => Sign},

    % 构建查询字符串
    QueryString = maps:fold(
        fun(Key, Value, Acc) ->
            KeyStr = binary_to_list(Key),
            ValueStr = binary_to_list(Value),
            Encoded = KeyStr ++ "=" ++ uri_string:quote(ValueStr),
            case Acc of
                "" -> Encoded;
                _ -> Acc ++ "&" ++ Encoded
            end
        end,
        "",
        RequestParamsWithSign
    ),

    % 发送GET请求
    UrlWithParams = <<Domain/binary, "/v2/common/estimatePriceWithDetail?", (list_to_binary(QueryString))/binary>>,
    logger:notice("曹操预估请求URL: ~p", [UrlWithParams]),
    
    case http_client:get(UrlWithParams) of
        {ok, 200, ResponseBody} ->
            % 解析响应
            try jsx:decode(ResponseBody, [return_maps]) of
                #{<<"code">> := 200, <<"data">> := Data, <<"success">> := true} ->
                    % 提取车型和价格信息
                    CarTypes = extract_car_types(Data),
                    {ok, CarTypes};
                #{<<"code">> := Code, <<"message">> := Message} ->
                    % 处理API返回的错误信息
                    {error, {api_error, Code, Message}};
                OtherResponse ->
                    % 处理其他未知格式的响应
                    logger:error("未知响应格式: ~p", [OtherResponse]),
                    {error, {unknown_response_format, OtherResponse}}
            catch
                Type:Reason:Stack ->
                    % 记录错误日志
                    logger:error("invalid_response: ~p:~p~n~p", [Type, Reason, Stack]),
                    {error, invalid_response}
            end;
        {ok, StatusCode, _} ->
            {error, {http_error, StatusCode}};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc 刷新配置
refresh_config() ->
    % 从Redis直接加载配置
    ProviderKey = <<"provider:caocao">>,
    case redis_client:get(ProviderKey) of
        {ok, ConfigJson} ->
            try
                % 解析JSON配置
                Config = jsx:decode(ConfigJson, [return_maps]),
                % 更新状态
                logger:notice("已加载曹操配置: ~p", [Config]),
                ets:insert(provider_config, {caocao, Config}),
                {ok, Config}
            catch
                Type:Reason:Stack ->
                    % 记录错误日志
                    logger:error(
                        "解析曹操配置失败: ~p:~p~n~p",
                        [Type, Reason, Stack]
                    ),
                    {error, {parse_error, Reason}}
            end;
        {error, Reason} ->
            % 记录错误日志
            logger:error("获取曹操配置失败: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc 获取配置
get_config() ->
    case ets:lookup(provider_config, caocao) of
        [{caocao, Config}] ->
            {ok, Config};
        [] ->
            refresh_config()
    end.

%% @doc 计算签名
%% 按照曹操专车API的签名规则计算签名
calculate_sign(Params, SignKey) ->
    % 添加签名密钥到参数中
    ParamsWithKey = maps:merge(SignKey, Params),
    % 按照键名排序
    SortedKeys = lists:sort(maps:keys(ParamsWithKey)),

    % 构建签名字符串 (key1value1key2value2...)
    SignStr = lists:foldl(
        fun(Key, Acc) ->
            Value = maps:get(Key, ParamsWithKey),
            <<Acc/binary, Key/binary, Value/binary>>
        end,
        <<>>,
        SortedKeys
    ),

    % 计算SHA1哈希
    HashBin = crypto:hash(sha, SignStr),
    
    % 转换为小写十六进制字符串
    list_to_binary([string:to_lower(io_lib:format("~2.16.0b", [X])) || <<X>> <= HashBin]).

%% @doc 提取车型和价格信息
extract_car_types(Data) ->
    % 将Data作为车型列表直接处理
    CarList = Data,

    % 转换为标准格式
    lists:map(
        fun(Car) ->
            % 提取车型信息
            CarType = maps:get(<<"carType">>, Car, 0),
            CarName = maps:get(<<"name">>, Car, <<>>),
            Price = maps:get(<<"price">>, Car, 0),
            OriginPrice = maps:get(<<"originPrice">>, Car, 0),
            Distance = maps:get(<<"distance">>, Car, 0),
            Duration = maps:get(<<"duration">>, Car, 0),
            Details = maps:get(<<"detail">>, Car, []),

            % 转换详情为标准格式
            PriceDetails = lists:foldl(
                fun(Item, Acc) ->
                    Code = maps:get(<<"chargeCode">>, Item, <<>>),
                    Amount = maps:get(<<"amount">>, Item, 0),
                    Desc = maps:get(<<"chargeDesc">>, Item, <<>>),
                    Acc#{Code => #{amount => Amount, desc => Desc}}
                end,
                #{},
                Details
            ),

            % 构建标准格式
            #{
                provider => <<"caocao">>,
                car_type => CarType,
                car_name => CarName,
                price => Price,
                original_price => OriginPrice,
                currency => <<"CNY">>,
                distance => Distance,
                duration => Duration,
                details => PriceDetails
            }
        end,
        CarList
    ).
