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

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 获取路线距离和时间
%% 调用腾讯地图API获取两点之间的驾车路线距离和时间
get_distance_duration(Start, End) ->
    % 从应用配置获取腾讯地图API配置
    case get_map_config() of
        {ok, Config} ->
            % 提取API URL和密钥, 转换为二进制格式
            % 构建请求URL
            Url = build_request_url(Start, End,
                list_to_binary(proplists:get_value(api_url, Config)),
                list_to_binary(proplists:get_value(key, Config))),
            % 发送请求
            case http_client:get(Url) of
                {ok, 200, ResponseBody} ->
                    % 解析响应
                    parse_response(ResponseBody, Start, End);
                {ok, StatusCode, _} ->
                    logger:error("腾讯地图API请求失败 [Start: ~p, End: ~p]: HTTP错误 ~p", 
                        [Start, End, StatusCode]),
                    {error, {http_error, StatusCode}};
                {error, Reason} ->
                    logger:error("腾讯地图API请求发送失败 [Start: ~p, End: ~p]: ~p", 
                        [Start, End, Reason]),
                    {error, {request_failed, Reason}}
            end;
        {error, Reason} ->
            logger:error("获取腾讯地图配置失败: ~p", [Reason]),
            {error, Reason}
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc 获取地图配置
%% 从应用配置获取腾讯地图API配置
get_map_config() ->
    % 从应用环境变量获取配置
    case application:get_env(external_trans_service, tencent_map) of
        {ok, Config} ->
            {ok, Config};
        undefined ->
            logger:error("腾讯地图配置未设置"),
            {error, config_not_found}
    end.

%% @doc 构建请求URL
%% 构建腾讯地图API请求URL
build_request_url(Start, End, ApiUrl, ApiKey) ->
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
    <<ApiUrl/binary, "?", QueryString/binary>>.

%% @doc 解析响应
%% 解析腾讯地图API响应
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
            logger:error("腾讯地图API返回错误 [Start: ~p, End: ~p]: ~p - ~p", 
                [Start, End, Status, Message]),
            {error, {api_error, Status, Message}}
    catch
        Type:Error:Stack ->
            logger:error("腾讯地图API响应解析失败 [Start: ~p, End: ~p]: ~p:~p~n~p", 
                [Start, End, Type, Error, Stack]),
            {error, invalid_response}
    end.

%% @doc 缓存结果
%% 将查询结果缓存到Redis
cache_result(Start, End, Result) ->
    % 构建缓存键
    Key = <<"map:route:", Start/binary, ":", End/binary>>,
    % 序列化结果
    ResultJson = jsx:encode(Result),

    % 存储到Redis，设置过期时间为1小时
    case redis_client:setex(Key, 3600, ResultJson) of
        {ok, _} -> ok;
        {error, Reason} -> 
            logger:error("缓存地图路线结果失败 [Start: ~p, End: ~p]: ~p", 
                [Start, End, Reason]),
            {error, Reason}
    end.
