-module(map_service).
-include("../include/records.hrl").

-export([get_route_info/2]).

%% @doc 从腾讯地图API获取路线信息
%% @param StartPoint 起点坐标
%% @param EndPoint 终点坐标
%% @returns {ok, #route_info{}} | {error, Reason}
get_route_info(StartPoint, EndPoint) ->
    try
        % 获取API配置
        {ok, MapApiConfig} = application:get_env(external_trans_service, tencent_map_api),
        BaseUrl = proplists:get_value(base_url, MapApiConfig),
        ApiKey = proplists:get_value(key, MapApiConfig),
        
        % 构建请求URL
        Url = build_route_url(BaseUrl, StartPoint, EndPoint, ApiKey),
        
        % 发起HTTP请求
        case hackney:get(Url, [], <<>>, [with_body]) of
            {ok, 200, _Headers, Body} ->
                % 解析响应
                Response = jsx:decode(Body, [return_maps]),
                Status = maps:get(<<"status">>, Response),
                
                if
                    Status =:= 0 ->
                        % 提取路线信息
                        Result = maps:get(<<"result">>, Response),
                        Routes = maps:get(<<"routes">>, Result),
                        [FirstRoute | _] = Routes,
                        
                        % 获取距离和时间
                        Distance = maps:get(<<"distance">>, FirstRoute),
                        Duration = maps:get(<<"duration">>, FirstRoute),
                        
                        % 返回路线信息
                        {ok, #route_info{
                            distance = float(Distance),
                            duration = Duration
                        }};
                    true ->
                        % API返回错误
                        Message = maps:get(<<"message">>, Response, <<"unknown error">>),
                        {error, {api_error, Message}}
                end;
            
            {ok, StatusCode, _Headers, Body} ->
                % 处理HTTP错误
                {error, {http_error, StatusCode, Body}};
                
            {error, Reason} ->
                % 处理请求错误
                {error, {request_error, Reason}}
        end
    catch
        E:R:S ->
            % 处理异常
            error_logger:error_msg("Map service error: ~p:~p~n~p~n", [E, R, S]),
            {error, map_service_error}
    end.

%% @doc 构建腾讯地图路线规划API URL
%% @private
%% @param BaseUrl API基础URL
%% @param StartPoint 起点坐标
%% @param EndPoint 终点坐标
%% @param ApiKey API密钥
%% @returns string() 完整URL
build_route_url(BaseUrl, StartPoint, EndPoint, ApiKey) ->
    % 构建起点坐标参数
    From = io_lib:format("~.6f,~.6f", [StartPoint#point.latitude, StartPoint#point.longitude]),
    
    % 构建终点坐标参数
    To = io_lib:format("~.6f,~.6f", [EndPoint#point.latitude, EndPoint#point.longitude]),
    
    % 构建完整URL
    binary_to_list(iolist_to_binary([
        BaseUrl, "/ws/direction/v1/driving?",
        "from=", From,
        "&to=", To,
        "&key=", ApiKey
    ])).