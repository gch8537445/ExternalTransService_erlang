%%%-------------------------------------------------------------------
%%% @doc
%%% 预估价HTTP处理器
%%% 处理预估价HTTP请求，解析请求参数，调用预估价服务，返回结果
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
%% 处理HTTP请求，解析JSON请求体，调用预估价服务，返回JSON响应
init(Req0, State) ->
    % 读取请求体
    {ok, Body, Req1} = cowboy_req:read_body(Req0),

    try
        % 解析JSON请求体
        Params = jsx:decode(Body, [return_maps]),

        % 直接调用order_service
        Response = order_service:create_order(Params),

        % 生成响应
        case Response of
            {ok, Result} ->
                Req2 = cowboy_req:reply(200, #{
                    <<"content-type">> => <<"application/json">>
                }, jsx:encode(#{
                    success => true,
                    data => Result
                }), Req1),
                {ok, Req2, State};
            {error, Reason} ->
                Req2 = cowboy_req:reply(400, #{
                    <<"content-type">> => <<"application/json">>
                }, jsx:encode(#{
                    success => false,
                    error => Reason
                }), Req1),
                {ok, Req2, State}
        end
    catch
        _:badarg ->
            % 处理JSON解析错误
            JSXError = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{
                success => false,
                error => invalid_json_format
            }), Req1),
            {ok, JSXError, State};
        _:OtherReason ->
            % 处理其他错误
            error_logger:error_msg("订单处理出错: ~p~n", [OtherReason]),
            OtherError = cowboy_req:reply(500, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{
                success => false,
                error => internal_server_error
            }), Req1),
            {ok, OtherError, State}
    end.

%% @doc 终止处理器
terminate(_Reason, _Req, _State) ->
    ok.

%%====================================================================
%% 内部函数
%%====================================================================