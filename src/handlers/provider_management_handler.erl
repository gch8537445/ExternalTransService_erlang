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
    Result = case Operation of
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