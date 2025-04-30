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
    Response = 
        case {Path} of
            {<<"/api/order/estimate_price">>} -> % 预估价
                order_service:estimate_price(Params);
            {<<"/api/order/create_order">>} -> % 创建订单
                order_service:create_order(Params);
            _ ->
                {error, path_error}
        end,
    
    % 生成响应
    case Response of
        {ok, Result} ->
            SuccessResponse = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{
                    success => true,
                    data => Result
                }), Req1),
            {ok, SuccessResponse, State};
        {error, Reason} ->
            FailResponse = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{
                    success => false,
                    error => Reason
                }), Req1),
            {ok, FailResponse, State};
        _ ->
            % 处理其他错误
            ErrorResponse = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{
                    success => false,
                    error => Response
                }), Req1),
            {ok, ErrorResponse, State}
    end.

%% @doc 终止处理器
terminate(_Reason, _Req, _State) ->
    ok.

%%====================================================================
%% 内部函数
%%====================================================================