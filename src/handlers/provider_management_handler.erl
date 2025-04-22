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
                handle_load_provider(RequestData);
            <<"unload_provider">> -> 
                handle_unload_provider(RequestData);
            <<"list_all_providers">> -> 
                handle_list_all_providers();
            <<"reload_all_providers">> -> 
                handle_reload_all_providers();
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
    Response = handle_list_all_providers(),
    
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

%% @doc 处理加载运力提供商
handle_load_provider(RequestData) ->
    % 获取提供商名称
    ProviderName = maps:get(<<"provider_name">>, RequestData, undefined),
    % 校验请求参数
    case ProviderName of
        undefined ->
            {error, missing_provider_name};
        _ ->
            % 转换为atom
            ProviderModule = binary_to_atom(ProviderName, utf8),
            % 加载提供商
            case provider_manager:load_provider(ProviderModule) of
                {ok, _Pid} ->
                    {ok, #{message => <<"运力提供商加载成功"/utf8>>, provider => ProviderName}};
                {error, {already_started, _}} ->
                    {error, provider_already_exists};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 处理卸载运力提供商
handle_unload_provider(RequestData) ->
    % 获取提供商名称
    ProviderName = maps:get(<<"provider_name">>, RequestData, undefined),
    % 校验请求参数
    case ProviderName of
        undefined ->
            {error, missing_provider_name};
        _ ->
            % 转换为atom
            ProviderModule = binary_to_atom(ProviderName, utf8),
            % 卸载提供商
            case provider_manager:unload_provider(ProviderModule) of
                ok ->
                    {ok, #{message => <<"运力提供商卸载成功"/utf8>>, provider => ProviderName}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 列出所有提供商
handle_list_all_providers() ->
    try
        % 获取所有提供商
        Providers = provider_manager:get_all_providers(),
        % 转换为二进制
        ProvidersBinary = [atom_to_binary(P, utf8) || P <- Providers],
        {ok, #{providers => ProvidersBinary}}
    catch
        _:Reason ->
            {error, Reason}
    end.

%% @doc 重新加载所有提供商
handle_reload_all_providers() ->
    try
        % 加载所有提供商
        Result = provider_manager:load_all_providers(),
        {ok, #{result => Result}}
    catch
        _:Reason ->
            {error, Reason}
    end.

%% @doc 终止处理器
terminate(_Reason, _Req, _State) ->
    ok. 