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
    estimate_price/1,
    create_order/1
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

    % 获取配置
    Domain = maps:get(<<"CAOCAO_DOMAIN">>, Config),
    ClientId = maps:get(<<"CAOCAO_CLIENT_ID">>, Config),
    SignKey = maps:get(<<"CAOCAO_SIGN_KEY">>, Config),
    Url = <<Domain/binary, "/v2/common/estimatePriceWithDetail?">>,

    % 提取参数
    Start = maps:get(<<"start_location">>, Params),
    End = maps:get(<<"end_location">>, Params),

    % 构建请求参数
    [StartLat, StartLng] = binary:split(Start, <<",">>),
    [EndLat, EndLng] = binary:split(End, <<",">>),

    % 构建请求体
    RequestParams = #{
        <<"client_id">> => ClientId,
        <<"sign_key">> => SignKey,
        <<"timestamp">> => integer_to_binary(erlang:system_time(millisecond)),
        <<"from_latitude">> => StartLat,
        <<"from_longitude">> => StartLng,
        <<"to_latitude">> => EndLat,
        <<"to_longitude">> => EndLng,
        <<"car_type">> => <<"2,3,5">>,
        <<"city_code">> => maps:get(<<"city_code">>, Config, <<"0411">>),
        <<"order_type">> => <<"1">>
    },

    case make_request(get, Url, RequestParams) of
        {ok, Data} ->
            % 提取车型和价格信息
            CarTypes = extract_car_types(Data),
            {ok, CarTypes};

        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

%% 发起叫车请求
create_order(Params) ->
    {ok, Config} = get_config(),
    % 获取配置
    Domain = maps:get(<<"CAOCAO_DOMAIN">>, Config),
    ClientId = maps:get(<<"CAOCAO_CLIENT_ID">>, Config),
    SignKey = maps:get(<<"CAOCAO_SIGN_KEY">>, Config),
    Url = <<Domain/binary, "/v2/common/orderCar?">>,

    % 构造基本参数
    BaseParams = #{
        <<"client_id">> => ClientId,
        <<"sign_key">> => SignKey,
        <<"timestamp">> => integer_to_binary(erlang:system_time(millisecond)),
        <<"from_latitude">> => maps:get(<<"from_latitude">>, Params),
        <<"from_longitude">> => maps:get(<<"from_longitude">>, Params),
        <<"to_latitude">> => maps:get(<<"to_latitude">>, Params),
        <<"to_longitude">> => maps:get(<<"to_longitude">>, Params),
        <<"car_type">> => maps:get(<<"car_type">>, Params),
        <<"ext_order_id">> => maps:get(<<"ext_order_id">>, Params),
        <<"city_code">> => maps:get(<<"city_code">>, Params),
        <<"order_type">> => maps:get(<<"order_type">>, Params)
    },

    OptionalFields = [
        {<<"passenger_phone">>, <<"passenger_phone">>},
        {<<"passenger_name">>, <<"passenger_name">>},
        {<<"caller_phone">>, <<"caller_phone">>},
        {<<"estimate_price">>, <<"estimate_price">>},
        {<<"estimate_price_key">>, <<"estimate_price_key">>},
        {<<"departure_time">>, <<"departure_time">>},
        {<<"start_name">>, <<"start_name">>},
        {<<"start_address">>, <<"start_address">>},
        {<<"end_name">>, <<"end_name">>},
        {<<"end_address">>, <<"end_address">>},
        {<<"order_latitude">>, <<"order_latitude">>},
        {<<"order_longitude">>, <<"order_longitude">>},
        {<<"sms_policy">>, <<"sms_policy">>},
        {<<"flight_no">>, <<"flight_no">>},
        {<<"flight_departure_time">>, <<"flt_takeoff_time">>},
        {<<"dynamic_rule_id">>, <<"dynamic_rule_id">>},
        {<<"hide_phone">>, <<"passenger_hide_phone">>},
        {<<"start_poi_id">>, <<"start_poi_id">>},
        {<<"end_poi_id">>, <<"end_poi_id">>},
        {<<"accept_cp_driver">>, <<"accept_cp_driver">>}
    ],

    % 添加其他可选参数
    FullParams = add_optional_params(BaseParams, OptionalFields, Params),
    make_request(post, Url, FullParams).

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc 刷新配置
refresh_config() ->
    % 从Redis直接加载配置
    ProviderKey = <<"provider:caocao">>,
    case redis_client:get(ProviderKey) of
        {ok, ConfigJson} ->
            % 解析JSON配置
            Config = jsx:decode(ConfigJson, [return_maps]),
            % 更新状态
            logger:notice("已加载曹操配置: ~p", [Config]),
            ets:insert(provider_config, {caocao, Config}),
            {ok, Config};
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
            PriceKey = maps:get(<<"priceKey">>, Car, []),
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
                price_key => PriceKey,
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

%% 通用请求函数
make_request(Method, Url, Params) ->
    % 添加签名
    ParamsWithoutKey = maps:remove(<<"sign_key">>, Params),
    Sign = calculate_sign(ParamsWithoutKey, #{<<"sign_key">> => maps:get(<<"sign_key">>, Params)}),
    ParamsWithSign = ParamsWithoutKey#{<<"sign">> => Sign},

    % 构建URL或请求体
    Request = case Method of
                  get ->
                      url_with_params(Url, ParamsWithSign);
                  post ->
                      url_with_params(Url, ParamsWithSign)
              end,

    logger:notice("make_request ------ URL: ~p", [Request]),

    % 发送请求
    case apply_request(Method, Request) of
        {ok, 200, ResponseBody} ->
            case jsx:decode(ResponseBody, [return_maps]) of
                #{<<"code">> := 200, <<"data">> := Data} ->
                    {ok, Data};
                ResponseMap ->
                    {error, ResponseMap}
            end;
        {error, RequestError} ->
            {error, RequestError}
    end.

%% 添加可选参数
add_optional_params(BaseParams, OptionalFields, OrderParams) ->
    lists:foldl(
        fun({Key, ApiKey}, Acc) ->
                case maps:find(Key, OrderParams) of
                    {ok, Value} when Value =/= undefined, Value =/= "" ->
                        maps:put(ApiKey, Value, Acc);
                    _ ->
                        Acc
                end
            end,
        BaseParams,
        OptionalFields
    ).

%% HTTP请求执行
apply_request(get, Url) ->
    http_client:get(Url);
apply_request(post, Url) ->
    http_client:post(Url);
apply_request(post, {Url, Body}) ->
    http_client:post(Url, Body);
apply_request(post, Url) when is_binary(Url) ->
    % 当 POST 请求只有 URL 没有请求体时
    http_client:post(Url, <<>>).

%% 构建带参数的URL
url_with_params(Url, Params) ->
    QueryString = maps:fold(
        fun(Key, Value, Acc) ->
            KeyStr = binary_to_list(Key),
            ValueStr = to_string(Value),
            Encoded = KeyStr ++ "=" ++ uri_string:quote(ValueStr),
            case Acc of
                "" -> Encoded;
                _ -> Acc ++ "&" ++ Encoded
            end
        end,
        "",
        Params
    ),
    <<Url/binary, (list_to_binary(QueryString))/binary>>.

%% 转换为字符串
to_string(V) when is_binary(V) -> binary_to_list(V);
to_string(V) when is_integer(V) -> integer_to_list(V);
to_string(V) when is_float(V) -> float_to_list(V);
to_string(V) when is_atom(V) -> atom_to_list(V);
to_string(V) -> V.