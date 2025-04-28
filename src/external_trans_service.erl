%%%-------------------------------------------------------------------
%%% @doc
%%% 外部运力服务应用入口
%%% @end
%%%-------------------------------------------------------------------
-module(external_trans_service).
-behaviour(application).

%% 应用程序回调函数
-export([start/2, stop/1]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 启动应用程序
%% 初始化应用程序，启动HTTP服务器和顶级监督者
start(_StartType, _StartArgs) ->
    % 定义HTTP路由
    Dispatch = cowboy_router:compile([
        {'_', [
            % 运力提供商管理API路由
            {"/api/provider", provider_management_handler, []},
            % 预估价API路由
            {"/api/estimate_price", estimate_price_handler, []},
            % 订单API路由
            {"/api/order/create_order", order_handler, []}
        ]}
    ]),

    % 启动HTTP服务器
    {ok, _} = cowboy:start_clear(
        http_listener,
        [{port, element(2, application:get_env(external_trans_service, http_port))}],% 监听端口
        #{env => #{dispatch => Dispatch}}
    ),

    % 启动顶级监督者
    external_trans_service_sup:start_link().

%% @doc 停止应用程序
stop(_State) ->
    % 停止HTTP服务器
    ok = cowboy:stop_listener(http_listener),
    ok.

%%====================================================================
%% 内部函数
%%====================================================================
