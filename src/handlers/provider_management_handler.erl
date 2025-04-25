%%%-------------------------------------------------------------------
%%% @doc
%%% 运力提供商管理API处理器
%%% 提供热插拔运力功能
%%% @end
%%%-------------------------------------------------------------------
-module(provider_management_handler).
-behaviour(cowboy_handler).

%% API
-export([init/2, terminate/3]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 初始化处理器
init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    {ok, handle_request(Method, Req0), State}.

%% @doc 处理请求
handle_request(<<"POST">>, Req0) ->
    % 解析请求体JSON
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try
        % 解析JSON数据
        RequestData = jsx:decode(Body, [return_maps]),
        % 获取操作类型
        Operation = maps:get(<<"operation">>, RequestData, undefined),
        % 处理不同的操作
        Response = case Operation of
            <<"load_provider">> ->
                provider_manager_service:load_provider(binary_to_atom(maps:get(<<"provider_module">>, RequestData, undefined), utf8));
            <<"unload_provider">> ->
                provider_manager_service:unload_provider(binary_to_atom(maps:get(<<"provider_module">>, RequestData, undefined), utf8));
            <<"list_providers">> ->
                provider_manager_service:get_all_providers();
            <<"reload_all_providers">> ->
                provider_manager_service:load_all_providers();
            _ ->
                {error, invalid_operation}
        end,

        % 生成响应
        case Response of
            {ok, Result} ->
                cowboy_req:reply(200, #{
                    <<"content-type">> => <<"application/json">>
                }, jsx:encode(#{
                    success => true,
                    data => Result
                }), Req1);
            {error, Reason} ->
                cowboy_req:reply(400, #{
                    <<"content-type">> => <<"application/json">>
                }, jsx:encode(#{
                    success => false,
                    error => Reason
                }), Req1)
        end
    catch
        _:_ ->
            % 处理JSON解析错误
            cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{
                success => false,
                error => invalid_json
            }), Req1)
    end;

handle_request(<<"GET">>, Req) ->
    % 列出所有提供商
    Response = provider_manager_service:get_all_providers(),

    % 生成响应
    case Response of
        {ok, Result} ->
            cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{
                success => true,
                data => Result
            }), Req);
        {error, Reason} ->
            cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{
                success => false,
                error => Reason
            }), Req)
    end;

handle_request(_, Req) ->
    % 仅支持GET和POST方法
    cowboy_req:reply(405, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(#{
        success => false,
        error => method_not_allowed
    }), Req).

%% @doc 终止处理器
terminate(_Reason, _Req, _State) ->
    ok.