%%%-------------------------------------------------------------------
%%% @doc
%%% 预估价HTTP处理器
%%% 处理预估价HTTP请求，解析请求参数，调用预估价服务，返回结果
%%% @end
%%%-------------------------------------------------------------------
-module(estimate_price_handler).
-behavior(cowboy_handler).

%% Cowboy回调函数
-export([init/2, terminate/3]).

%%====================================================================
%% Cowboy回调函数
%%====================================================================

%% @doc 初始化处理器
%% 处理HTTP请求，解析JSON请求体，调用预估价服务，返回JSON响应
init(Req0, State) ->
    % 读取请求体
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    % 解析JSON请求体
    Params = jsx:decode(Body, [return_maps]),
    % 调用order_service
    Response = order_service:create_order(Params),
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
        Error ->
            % 处理其他错误
            ErrorResponse = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{
                    success => false,
                    error => Error
                }), Req1),
            {ok, ErrorResponse, State}
    end.

%% @doc 终止处理器
terminate(_Reason, _Req, _State) ->
    ok.

%%====================================================================
%% 内部函数
%%====================================================================