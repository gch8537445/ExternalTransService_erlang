%%%-------------------------------------------------------------------
%%% @doc
%%% 腾讯地图服务
%%% 负责与腾讯地图API通信，获取路线距离和时间等信息
%%% @end
%%%-------------------------------------------------------------------
-module(tencent_map_service).

%% API
-export([get_distance_duration/2]).

%% 内部导出函数，供测试使用
-export([parse_response/3, cache_result/3]).

-define(TENCENT_MAP_API_URL, <<"https://apis.map.qq.com/ws/direction/v1/driving/">>).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 获取路线距离和时间
%% 调用腾讯地图API获取两点之间的驾车路线距离和时间
-spec get_distance_duration(binary(), binary()) -> 
    {ok, map()} | {error, term()}.
get_distance_duration(Start, End) ->
    % 从Redis获取腾讯地图API配置
    case get_map_config() of
        {ok, Config} ->
            % 提取API密钥
            ApiKey = maps:get(<<"key">>, Config, <<"">>),

            % 构建请求URL
            Url = build_request_url(Start, End, ApiKey),

            % 发送请求
            case http_client:get(Url) of
                {ok, 200, ResponseBody} ->
                    % 解析响应
                    parse_response(ResponseBody, Start, End);
                {ok, StatusCode, _} ->
                    {error, {http_error, StatusCode}};
                {error, Reason} ->
                    {error, {request_failed, Reason}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc 获取地图配置
%% 从Redis获取腾讯地图API配置
-spec get_map_config() -> {ok, map()} | {error, term()}.
get_map_config() ->
    % 从Redis获取配置
    case redis_client:get(<<"config:tencent_map">>) of
        {ok, ConfigJson} ->
            try jsx:decode(ConfigJson, [return_maps]) of
                Config -> {ok, Config}
            catch
                _:_ -> {error, invalid_map_config}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 构建请求URL
%% 构建腾讯地图API请求URL
-spec build_request_url(binary(), binary(), binary()) -> binary().
build_request_url(Start, End, ApiKey) ->
    % 分割起点和终点坐标
    [StartLat, StartLng] = binary:split(Start, <<",">>),
    [EndLat, EndLng] = binary:split(End, <<",">>),

    % 构建请求参数
    Params = [
        {<<"from">>, <<StartLat/binary, ",", StartLng/binary>>},
        {<<"to">>, <<EndLat/binary, ",", EndLng/binary>>},
        {<<"key">>, ApiKey},
        {<<"output">>, <<"json">>}
    ],

    % 构建查询字符串
    QueryString = lists:foldl(
        fun({Key, Value}, Acc) ->
            if
                Acc =:= <<>> ->
                    <<Key/binary, "=", Value/binary>>;
                true ->
                    <<Acc/binary, "&", Key/binary, "=", Value/binary>>
            end
        end,
        <<>>,
        Params
    ),

    % 构建完整URL
    <<?TENCENT_MAP_API_URL/binary, "?", QueryString/binary>>.

%% @doc 解析响应
%% 解析腾讯地图API响应
-spec parse_response(binary(), binary(), binary()) -> {ok, map()} | {error, term()}.
parse_response(ResponseBody, Start, End) ->
    try jsx:decode(ResponseBody, [return_maps]) of
        #{<<"status">> := 0, <<"result">> := Result} ->
            % 提取路线信息
            #{<<"routes">> := [Route | _]} = Result,

            % 提取距离和时间
            #{<<"distance">> := Distance, <<"duration">> := Duration} = Route,

            % 构建结果
            {ok, #{
                distance => Distance,  % 单位：米
                duration => Duration,  % 单位：秒
                start_location => Start,
                end_location => End
            }};
        #{<<"status">> := Status, <<"message">> := Message} ->
            {error, {api_error, Status, Message}}
    catch
        _:_ ->
            {error, invalid_response}
    end.

%% @doc 缓存结果
%% 将查询结果缓存到Redis
-spec cache_result(binary(), binary(), map()) -> ok | {error, term()}.
cache_result(Start, End, Result) ->
    % 构建缓存键
    Key = <<"map:route:", Start/binary, ":", End/binary>>,

    % 序列化结果
    ResultJson = jsx:encode(Result),

    % 存储到Redis，设置过期时间为1小时
    case redis_client:setex(Key, 3600, ResultJson) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.
