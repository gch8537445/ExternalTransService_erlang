%%%-------------------------------------------------------------------
%%% @doc
%%% 运力提供商管理器
%%% 管理所有运力提供商，提供统一的接口给其他模块调用
%%% @end
%%%-------------------------------------------------------------------
-module(provider_manager).
-behaviour(gen_server).

%% gen_server回调函数
-export([
    init/1,
    start_link/0,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% API
-export([
    get_all_providers/0,
    estimate_price/3,
    load_provider/1,
    unload_provider/1,
    load_all_providers/0
]).

%% 内部常量
-define(ETS_TABLE, provider_config).
-define(PROVIDER_KEY, provider).
-define(PROVIDER_DIR, "src/services/providers/sub_providers"). %提供商目录

%%====================================================================
%% gen_server回调函数
%%====================================================================
%% @doc 启动服务器
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 初始化服务器
init([]) ->
    % 注册运力提供商启动和停止事件
    ok = net_kernel:monitor_nodes(true),

    % 创建提供者配置ETS表
    case ets:info(?ETS_TABLE) of
        undefined ->
            ets:new(?ETS_TABLE, [named_table, public, set]);
        _ ->
            ok
    end,
    % 初始化提供商列表
    ets:insert(?ETS_TABLE, {?PROVIDER_KEY, []}),

    % 启动后自动加载所有运力提供商
    load_all_providers(),
    % 返回初始状态（不再需要在state中存储提供商列表）
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.
handle_cast(_Request, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
%% @doc 终止服务器
terminate(_Reason, _State) ->
    ok.
%% @doc 代码更新
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% API 函数
%%====================================================================
%% @doc 获取所有运力提供商
get_all_providers() ->
    % 直接从ETS表中获取提供商列表
    case ets:lookup(?ETS_TABLE, ?PROVIDER_KEY) of
        [{?PROVIDER_KEY, Providers}] ->
            {ok, Providers};
        [] ->
            {ok, []}
    end.

%% @doc 调用所有运力提供商的预估价接口
estimate_price(Start, End, UserId) ->
    % 创建请求参数
    Params = #{
        start_location => Start,
        end_location => End,
        user_id => UserId
    },

    % 获取提供商列表
    {ok, Providers} = get_all_providers(),
    
    % 并行发送请求
    Results = pmap(
        fun(ProviderModule) ->
            try
                % 直接调用提供者模块的estimate_price函数
                case ProviderModule:estimate_price(Params) of
                    {ok, Result} ->
                        {ProviderModule, Result};
                    {error, Reason} ->
                        {ProviderModule, {error, Reason}}
                end
            catch
                exit:{timeout, _} ->
                    {ProviderModule, {error, timeout}};
                Type:ErrorReason:Stack ->
                    logger:error(
                        "Provider ~p estimate failed: ~p:~p~n~p",
                        [ProviderModule, Type, ErrorReason, Stack]
                    ),
                    {ProviderModule, {error, internal_error}}
            end
        end,
        Providers
    ),

    % 构建结果
    {ok, Results}.

%% @doc 动态加载一个运力提供商
%% ProviderModule: 运力提供商代码模块名
load_provider(ProviderModule) ->
    case ProviderModule of
        undefined ->
            {error, missing_provider_module};
        _ ->
            % 获取当前提供商列表
            {ok, Providers} = get_all_providers(),

            % 检查提供商是否已经存在
            case lists:member(ProviderModule, Providers) of
                true ->
                    {error, provider_already_exists};
                false ->
                    AppDir = code:lib_dir(external_trans_service),
                    SrcFile = filename:join([AppDir, ?PROVIDER_DIR, atom_to_list(ProviderModule) ++ ".erl"]),

                    case compile:file(SrcFile, [binary]) of
                        {ok, ProviderModule, Binary} ->
                            % 确保模块已加载
                            case code:load_binary(ProviderModule, SrcFile, Binary) of
                                {module, ProviderModule} ->
                                    % 模块已经加载，初始化提供者
                                    try
                                        % 调用提供者的init函数初始化
                                        case ProviderModule:init([]) of
                                            {ok, _} ->
                                                % 提供者初始化成功，将其添加到ETS表中
                                                NewProviders = [ProviderModule | Providers],
                                                ets:insert(?ETS_TABLE, {?PROVIDER_KEY, NewProviders}),
                                                {ok, #{
                                                    message => <<"运力提供商加载成功"/utf8>>,
                                                    provider_module => ProviderModule
                                                }};
                                            {error, InitError} ->
                                                {error, InitError}
                                        end
                                    catch
                                        Type:Error:Stack ->
                                            logger:error(
                                                "Provider ~p init failed: ~p:~p~n~p",
                                                [ProviderModule, Type, Error, Stack]
                                            ),
                                            {error, {init_failed, Error}}
                                    end;
                                {error, LoadError} ->
                                    % 模块加载失败
                                    logger:error("LoadError-----------------------~p", [LoadError]),
                                    {error, {module_load_failed, LoadError}}
                            end;
                        {error, CompileError} ->
                            % 模块编译失败
                            logger:error("CompileError-----------------------~p", [CompileError]),
                            {error, {module_compile_failed, CompileError}}
                    end
            end
    end.

%% @doc 动态卸载一个运力提供商
unload_provider(ProviderModule) ->
    case ProviderModule of
        undefined ->
            {error, missing_provider_module};
        _ ->
            % 获取当前提供商列表
            {ok, Providers} = get_all_providers(),

            % 检查提供商是否存在
            case lists:member(ProviderModule, Providers) of
                true ->
                    % 从列表中移除提供商
                    NewProviders = lists:delete(ProviderModule, Providers),
                    ets:insert(?ETS_TABLE, {?PROVIDER_KEY, NewProviders}),
                    {ok, #{
                        message => <<"运力提供商卸载成功"/utf8>>,
                        provider_code => ProviderModule
                    }};
                false ->
                    {error, provider_not_found}
            end
    end.

%% @doc 加载所有运力提供商
load_all_providers() ->
    % 遍历目录中的所有文件
    case file:list_dir(?PROVIDER_DIR) of
        {ok, Files} ->
            % 过滤出以_provider.erl结尾的文件
            ProviderFiles = lists:filter(
                fun(File) ->
                    case string:find(File, "_provider.erl", trailing) of
                        nomatch -> false;
                        _ -> true
                    end
                end,
                Files
            ),
            
            % 加载每个提供商
            {Results, Providers} = lists:foldl(
                fun(File, {ResultsAcc, ProvidersAcc}) ->
                    % 提取模块名（去除.erl后缀）
                    ProviderModule = list_to_atom(string:sub_string(File, 1, length(File) - 4)),
                    % 确保模块已加载
                    case code:ensure_loaded(ProviderModule) of
                        {module, ProviderModule} ->
                            % 初始化提供者
                            try
                                case ProviderModule:init([]) of
                                    {ok, _} ->
                                        % 监控提供商进程不再需要
                                        logger:notice("自动加载并初始化运力提供商: ~p", [ProviderModule]),
                                        % 更新提供商列表
                                        {[{ok, ProviderModule} | ResultsAcc], [ProviderModule | ProvidersAcc]};
                                    {error, Reason} ->
                                        logger:error("初始化运力提供商失败 ~p: ~p", [ProviderModule, Reason]),
                                        {[{error, {init_failed, ProviderModule, Reason}} | ResultsAcc], ProvidersAcc}
                                end
                            catch
                                Type:Error:Stack ->
                                    logger:error(
                                        "Provider ~p init failed: ~p:~p~n~p", 
                                        [ProviderModule, Type, Error, Stack]
                                    ),
                                    {[{error, {init_failed, ProviderModule, Error}} | ResultsAcc], ProvidersAcc}
                            end;
                        {error, Reason} ->
                            logger:error("加载运力提供商模块失败 ~p: ~p", [ProviderModule, Reason]),
                            {[{error, {load_failed, ProviderModule, Reason}} | ResultsAcc], ProvidersAcc}
                    end
                end,
                {[], []},
                ProviderFiles
            ),
            
            % 将提供商列表存储到ETS表中
            ets:insert(?ETS_TABLE, {?PROVIDER_KEY, Providers}),
            
            % 返回结果
            {ok, Results};
        {error, Reason} ->
            % 无法读取提供商目录
            logger:error("无法读取提供商目录: ~p", [Reason]),
            {error, {read_dir_failed, Reason}}
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc 并行映射函数
%% 并行执行函数F在列表L的每个元素上
pmap(F, L) ->
    Parent = self(),
    Refs = lists:map(
        fun(X) ->
            Ref = make_ref(),
            spawn_link(
                fun() ->
                    Parent ! {Ref, F(X)}
                end
            ),
            Ref
        end,
        L
    ),
    lists:map(
        fun(Ref) ->
            receive
                {Ref, Result} -> Result
            end
        end,
        Refs
    ).
