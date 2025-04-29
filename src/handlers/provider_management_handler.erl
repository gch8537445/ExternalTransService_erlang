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
    % 解析请求体JSON
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
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
                    error => internal_server_error
                }), Req1),
            {ok, ErrorResponse, State}
    end.

%% @doc 终止处理器
terminate(_Reason, _Req, _State) ->
    ok.