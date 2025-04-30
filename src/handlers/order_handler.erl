%%%-------------------------------------------------------------------
%%% @doc
%%% 订单HTTP处理器
%%% @end
%%%-------------------------------------------------------------------
-module(order_handler).
-behavior(cowboy_handler).

%% Cowboy回调函数
-export([init/2, terminate/3]).

%%====================================================================
%% Cowboy回调函数
%%====================================================================

%% @doc 初始化处理器
%% 处理HTTP请求，解析JSON请求体，根据操作类型调用订单服务，返回JSON响应
init(Req0, State) ->
    % 读取请求体
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    % 解析JSON请求体
    Params = jsx:decode(Body, [return_maps]),
    
    % 获取请求路径和方法以确定操作类型
    Path = cowboy_req:path(Req1),
    %Method = cowboy_req:method(Req1),
    
    % 根据路径和方法决定调用哪个服务方法
    Result =
        case Path of
            <<"/api/order/estimate_price">> -> % 预估价
                order_service:estimate_price(Params);
            <<"/api/order/create_order">> -> % 创建订单
                order_service:create_order(Params);
            _ ->
                {error, path_error}
        end,

    try
        Response = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, jsx:encode(Result), Req1),
        {ok, Response, State}
     catch
         _ ->
             logger:error("Result ~p", [Result]),
             ErrorResponse = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, Result, Req1),
             {ok, ErrorResponse, State}
     end.

%% @doc 终止处理器
terminate(_Reason, _Req, _State) ->
    ok.