%%%-------------------------------------------------------------------
%%% @doc
%%% 运力提供商管理器
%%% 管理所有运力提供商，提供统一的接口给其他模块调用
%%% @end
%%%-------------------------------------------------------------------
-module(provider_manager).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    get_all_providers/0,
    get_provider/1,
    estimate_price/3,
    load_provider/1,
    unload_provider/1,
    load_all_providers/0
]).

%% gen_server回调函数
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).
-define(PROVIDER_DIR, "src/providers/sub_providers").

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 启动服务器
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc 获取所有运力提供商
get_all_providers() ->
    gen_server:call(?SERVER, get_all_providers).

%% @doc 获取指定运力提供商的PID
get_provider(ProviderName) ->
    gen_server:call(?SERVER, {get_provider, ProviderName}).

%% @doc 调用所有运力提供商的预估价接口
estimate_price(Start, End, UserId) ->
    gen_server:call(?SERVER, {estimate_price, Start, End, UserId}).

%% @doc 动态加载一个运力提供商
load_provider(ProviderModule) ->
    gen_server:call(?SERVER, {load_provider, ProviderModule}).

%% @doc 动态卸载一个运力提供商
unload_provider(ProviderModule) ->
    gen_server:call(?SERVER, {unload_provider, ProviderModule}).

%% @doc 加载所有运力提供商
load_all_providers() ->
    gen_server:call(?SERVER, load_all_providers).

%%====================================================================
%% gen_server回调函数
%%====================================================================

%% @doc 初始化服务器
init([]) ->
    % 注册运力提供商启动和停止事件
    ok = net_kernel:monitor_nodes(true),
    ensure_provider_cache(),
    % 启动后自动加载所有运力提供商
    self() ! load_all_providers,
    {ok, []}.

%% @doc 处理同步调用
handle_call(get_all_providers, _From, State) ->
    % 从缓存中获取所有提供商名称
    Providers = get_providers_from_cache(),
    {reply, maps:keys(Providers), State};

handle_call({get_provider, ProviderName}, _From, State) ->
    % 从缓存中获取指定提供商的PID
    Providers = get_providers_from_cache(),
    case maps:find(ProviderName, Providers) of
        {ok, Pid} -> 
            % 验证PID是否仍然存活
            case is_process_alive(Pid) of
                true -> {reply, {ok, Pid}, State};
                false -> 
                    % 如果进程已死，从缓存中移除
                    remove_provider_from_cache(ProviderName),
                    {reply, {error, process_not_alive}, State}
            end;
        error -> {reply, {error, not_found}, State}
    end;

handle_call({get_provider_config, _ProviderName}, _From, State) ->
    % 此功能已弃用，各提供商从缓存中获取自己的配置
    {reply, {error, deprecated_function}, State};

handle_call({estimate_price, Start, End, UserId}, _From, State) ->
    % 从缓存中获取所有提供商
    Providers = maps:to_list(get_providers_from_cache()),
    % 创建请求参数
    Params = #{
        start_location => Start,
        end_location => End,
        user_id => UserId
    },

    % 并行发送请求
    Results = pmap(
        fun({ProviderName, Pid}) ->
            try
                % 验证进程是否存活
                case is_process_alive(Pid) of
                    true ->
                        % 调用提供商的预估价接口
                        case gen_server:call(Pid, {estimate_price, Params}, 5000) of
                            {ok, Result} ->
                                {ProviderName, Result};
                            {error, Reason} ->
                                {ProviderName, {error, Reason}}
                        end;
                    false ->
                        % 进程不存活，从缓存中移除
                        remove_provider_from_cache(ProviderName),
                        {ProviderName, {error, provider_not_available}}
                end
            catch
                exit:{timeout, _} ->
                    {ProviderName, {error, timeout}};
                Type:ErrorReason:Stack ->
                    logger:error(
                        "Provider ~p estimate failed: ~p:~p~n~p",
                        [ProviderName, Type, ErrorReason, Stack]
                    ),
                    {ProviderName, {error, internal_error}}
            end
        end,
        Providers
    ),

    % 构建结果
    {reply, {ok, Results}, State};

handle_call({load_provider, ProviderModule}, _From, State) ->
    % 动态加载运力提供商
    Reply = case code:ensure_loaded(ProviderModule) of
        {module, ProviderModule} ->
            % 模块已经加载，尝试启动
            provider_sup:start_provider(ProviderModule);
        {error, Reason} ->
            % 模块加载失败
            {error, {module_load_failed, Reason}}
    end,
    {reply, Reply, State};

handle_call({unload_provider, ProviderModule}, _From, State) ->
    % 动态卸载运力提供商
    Reply = provider_sup:stop_provider(ProviderModule),
    {reply, Reply, State};

handle_call(load_all_providers, _From, State) ->
    % 加载所有运力提供商
    Result = load_provider_modules(),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @doc 处理异步调用
handle_cast({register_provider, ProviderName, Pid}, State) ->
    % 注册提供商到缓存
    add_provider_to_cache(ProviderName, Pid),
    % 监控提供商进程，当其终止时自动移除
    erlang:monitor(process, Pid),
    {noreply, State};

handle_cast({unregister_provider, ProviderName}, State) ->
    % 从缓存中注销提供商
    remove_provider_from_cache(ProviderName),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

%% @doc 处理信息
handle_info(load_all_providers, State) ->
    % 启动时加载所有运力提供商
    load_provider_modules(),
    {noreply, State};

handle_info({nodeup, _Node}, State) ->
    % 节点上线，刷新提供商列表
    {noreply, State};

handle_info({nodedown, _Node}, State) ->
    % 节点下线，刷新提供商列表
    {noreply, State};

handle_info({'DOWN', _MonitorRef, process, Pid, _Reason}, State) ->
    % 当被监控的提供商进程终止时，从缓存中移除
    remove_provider_by_pid_from_cache(Pid),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc 终止服务器
terminate(_Reason, _State) ->
    ok.

%% @doc 代码更新
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc 加载所有提供商模块
load_provider_modules() ->
    % 获取提供商目录
    ProviderDir = ?PROVIDER_DIR,
    % 遍历目录中的所有文件
    case file:list_dir(ProviderDir) of
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
            Results = lists:map(
                fun(File) ->
                    % 提取模块名（去除.erl后缀）
                    ModuleName = list_to_atom(string:sub_string(File, 1, length(File) - 4)),
                    % 确保模块已加载
                    case code:ensure_loaded(ModuleName) of
                        {module, ModuleName} ->
                            % 启动提供商
                            case provider_sup:start_provider(ModuleName) of
                                {ok, _} -> 
                                    logger:notice("自动加载并启动运力提供商: ~p", [ModuleName]),
                                    {ok, ModuleName};
                                {error, {already_started, _}} ->
                                    logger:notice("运力提供商已经启动: ~p", [ModuleName]),
                                    {ok, ModuleName};
                                {error, Reason} ->
                                    logger:error("启动运力提供商失败 ~p: ~p", [ModuleName, Reason]),
                                    {error, {start_failed, ModuleName, Reason}}
                            end;
                        {error, Reason} ->
                            logger:error("加载运力提供商模块失败 ~p: ~p", [ModuleName, Reason]),
                            {error, {load_failed, ModuleName, Reason}}
                    end
                end,
                ProviderFiles
            ),
            
            {ok, Results};
        {error, Reason} ->
            logger:error("打开提供商目录失败: ~p", [Reason]),
            {error, {dir_open_failed, Reason}}
    end.

%% @doc 确保提供商缓存存在
ensure_provider_cache() ->
    ProviderCacheKey = provider_cache_key(),
    case redis_client:exists(ProviderCacheKey) of
        {ok, <<"0">>} ->
            % 缓存不存在，创建空映射
            redis_client:set(ProviderCacheKey, jsx:encode(#{}));
        {ok, <<"1">>} ->
            % 缓存已存在
            ok;
        {error, Reason} ->
            logger:error("Failed to check provider cache: ~p", [Reason]),
            % 创建空映射
            redis_client:set(ProviderCacheKey, jsx:encode(#{}))
    end.

%% @doc 获取提供商缓存键
provider_cache_key() ->
    <<"providers:list">>.

%% @doc 从缓存获取提供商映射
get_providers_from_cache() ->
    ProviderCacheKey = provider_cache_key(),
    case redis_client:get(ProviderCacheKey) of
        {ok, ProvidersJson} ->
            try
                % 从JSON解码为Erlang映射
                ProvidersMap = jsx:decode(ProvidersJson, [return_maps]),
                % 转换所有键为atom
                maps:fold(
                    fun(K, V, Acc) ->
                        % 转换键为atom
                        ProviderName = binary_to_atom(K, utf8),
                        % 转换PID字符串为PID
                        Pid = try list_to_pid(binary_to_list(V))
                              catch _:_ -> undefined
                              end,
                        % 只保留有效的PID
                        case is_pid(Pid) of
                            true -> Acc#{ProviderName => Pid};
                            false -> Acc
                        end
                    end,
                    #{},
                    ProvidersMap
                )
            catch
                _:_ -> #{}
            end;
        _ ->
            % 出错或缓存不存在，返回空映射
            #{}
    end.

%% @doc 将提供商添加到缓存
add_provider_to_cache(ProviderName, Pid) ->
    ProviderCacheKey = provider_cache_key(),
    % 获取当前提供商映射
    Providers = get_providers_from_cache(),
    % 添加新提供商
    NewProviders = Providers#{ProviderName => Pid},
    % 转换为JSON格式存储
    ProvidersJson = encode_providers_for_cache(NewProviders),
    % 存入缓存
    redis_client:set(ProviderCacheKey, ProvidersJson).

%% @doc 将提供商从缓存移除
remove_provider_from_cache(ProviderName) ->
    ProviderCacheKey = provider_cache_key(),
    % 获取当前提供商映射
    Providers = get_providers_from_cache(),
    % 移除指定提供商
    NewProviders = maps:remove(ProviderName, Providers),
    % 转换为JSON格式存储
    ProvidersJson = encode_providers_for_cache(NewProviders),
    % 存入缓存
    redis_client:set(ProviderCacheKey, ProvidersJson).

%% @doc 通过PID从缓存中移除提供商
remove_provider_by_pid_from_cache(Pid) ->
    % 获取所有提供商
    Providers = get_providers_from_cache(),
    % 找出与给定PID匹配的提供商
    ProvidersToRemove = maps:filter(
        fun(_Name, ProviderPid) -> ProviderPid =:= Pid end,
        Providers
    ),
    % 如果找到了匹配的提供商，移除它们
    case maps:size(ProvidersToRemove) of
        0 -> ok;
        _ ->
            % 创建新的提供商映射，移除匹配的提供商
            NewProviders = maps:filter(
                fun(_Name, ProviderPid) -> ProviderPid =/= Pid end,
                Providers
            ),
            % 存入缓存
            ProviderCacheKey = provider_cache_key(),
            ProvidersJson = encode_providers_for_cache(NewProviders),
            redis_client:set(ProviderCacheKey, ProvidersJson)
    end.

%% @doc 将提供商映射编码为JSON
encode_providers_for_cache(Providers) ->
    % 将PID转为字符串，键转为二进制
    ProvidersBinary = maps:fold(
        fun(K, V, Acc) ->
            % 转换键为二进制
            KeyBin = atom_to_binary(K, utf8),
            % 转换PID为字符串二进制
            ValBin = list_to_binary(pid_to_list(V)),
            Acc#{KeyBin => ValBin}
        end,
        #{},
        Providers
    ),
    % 编码为JSON
    jsx:encode(ProvidersBinary).

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
