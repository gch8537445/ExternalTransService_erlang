-module(external_trans_service_app).
-behaviour(application).

-export([start/2, stop/1]).

%% @doc 应用程序启动回调函数
%% @param Type 应用程序启动类型
%% @param Args 应用程序启动参数
%% @returns {ok, Pid} | {error, Reason}
start(_Type, _Args) ->
    % 获取HTTP服务器端口
    {ok, HttpPort} = application:get_env(external_trans_service, http_port),
    
    % 定义HTTP路由
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/price/estimate", price_estimate_handler, []}
        ]}
    ]),
    
    % 启动HTTP服务器
    {ok, _} = cowboy:start_clear(
        http_listener,
        [{port, HttpPort}],
        #{env => #{dispatch => Dispatch}}
    ),
    
    io:format("HTTP server started on port ~p~n", [HttpPort]),
    
    % 启动监督树
    external_trans_service_sup:start_link().

%% @doc 应用程序停止回调函数
%% @param State 应用程序状态
stop(_State) ->
    ok = cowboy:stop_listener(http_listener),
    ok.