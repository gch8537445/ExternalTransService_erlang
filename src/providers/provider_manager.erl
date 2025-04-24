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

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 启动服务器
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 获取所有运力提供商
get_all_providers() ->
    gen_server:call(?MODULE, get_all_providers).

%% @doc 调用所有运力提供商的预估价接口
estimate_price(Start, End, UserId) ->
    gen_server:call(?MODULE, {estimate_price, Start, End, UserId}).

%% @doc 动态加载一个运力提供商
%% ProviderModule: 运力提供商代码模块名
load_provider(ProviderModule) ->
    gen_server:call(?MODULE, {load_provider, ProviderModule}).

%% @doc 动态卸载一个运力提供商
unload_provider(ProviderModule) ->
    gen_server:call(?MODULE, {unload_provider, ProviderModule}).

%% @doc 加载所有运力提供商
load_all_providers() ->
    gen_server:call(?MODULE, load_all_providers).

%%====================================================================
%% gen_server回调函数
%%====================================================================

%% @doc 初始化服务器
init([]) ->
    % 注册运力提供商启动和停止事件
    ok = net_kernel:monitor_nodes(true),
    
    % 创建提供者配置ETS表
    case ets:info(provider_config) of
        undefined ->
            ets:new(provider_config, [named_table, public, set]);
        _ ->
            ok
    end,
    
    % 启动后自动加载所有运力提供商
    self() ! load_all_providers,
    {ok, #{
        % 运力提供商映射: ProviderModule => Pid
    }}.

%% @doc 处理同步调用
handle_call(get_all_providers, _From, State) ->
    % 返回所有提供商代码
    {reply, maps:keys(State), State};

handle_call({estimate_price, Start, End, UserId}, _From, State) ->
    % 从状态中获取所有提供商
    ProviderList = maps:to_list(State),
    
    % 创建请求参数
    Params = #{
        start_location => Start,
        end_location => End,
        user_id => UserId
    },

    % 并行发送请求
    Results = pmap(
        fun({ProviderModule, _Pid}) ->
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
        ProviderList
    ),

    % 构建结果
    {reply, {ok, Results}, State};

handle_call({load_provider, ProviderModule}, _From, State) ->
    % 动态加载运力提供商
    case maps:is_key(ProviderModule, State) of
        true ->
            {reply, {error, provider_already_exists}, State};
        false ->
            % 确保模块已加载
            case code:ensure_loaded(ProviderModule) of
                {module, ProviderModule} ->
                    % 模块已经加载，初始化提供者
                    try
                        % 调用提供者的init函数初始化
                        case ProviderModule:init([]) of
                            {ok, _} ->
                                % 提供者初始化成功，将其添加到状态中
                                % 使用模块名称作为键和值
                                NewState = State#{ProviderModule => ProviderModule},
                                {reply, {ok, started}, NewState};
                            {error, InitError} ->
                                {reply, {error, InitError}, State}
                        end
                    catch
                        Type:Error:Stack ->
                            logger:error(
                                "Provider ~p init failed: ~p:~p~n~p",
                                [ProviderModule, Type, Error, Stack]
                            ),
                            {reply, {error, {init_failed, Error}}, State}
                    end;
                {error, LoadError} ->
                    % 模块加载失败
                    {reply, {error, {module_load_failed, LoadError}}, State}
            end
    end;

handle_call({unload_provider, ProviderModule}, _From, State) ->
    % 动态卸载运力提供商
    case maps:find(ProviderModule, State) of
        {ok, _Pid} ->
            % 从状态中移除提供商映射
            NewState = maps:remove(ProviderModule, State),
            {reply, ok, NewState};
        error ->
            {reply, {error, provider_not_found}, State}
    end;

handle_call(load_all_providers, _From, State) ->
    % 加载所有运力提供商
    {Result, NewState} = load_provider_modules(State),
    {reply, Result, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

%% @doc 处理信息
handle_info(load_all_providers, State) ->
    % 启动时加载所有运力提供商
    {_, NewState} = load_provider_modules(State),
    {noreply, NewState};

handle_info({nodeup, _Node}, State) ->
    % 节点上线，刷新提供商列表
    {noreply, State};

handle_info({nodedown, _Node}, State) ->
    % 节点下线，刷新提供商列表
    {noreply, State};

handle_info({'DOWN', _MonitorRef, process, Pid, _Reason}, State) ->
    % 当被监控的提供商进程终止时，从状态中移除
    NewState = remove_provider_by_pid(Pid, State),
    {noreply, NewState};

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
load_provider_modules(State) ->
    % 获取提供商目录
    ProviderDir = "src/providers/sub_providers",
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
            {Results, NewState} = lists:foldl(
                fun(File, {ResultsAcc, StateAcc}) ->
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
                                        % 更新状态，将 ProviderModule => ProviderModule 添加到映射中
                                        NewStateAcc = StateAcc#{ProviderModule => ProviderModule},
                                        {[{ok, ProviderModule} | ResultsAcc], NewStateAcc};
                                    {error, Reason} ->
                                        logger:error("初始化运力提供商失败 ~p: ~p", [ProviderModule, Reason]),
                                        {[{error, {init_failed, ProviderModule, Reason}} | ResultsAcc], StateAcc}
                                end
                            catch
                                Type:Error:Stack ->
                                    logger:error(
                                        "Provider ~p init failed: ~p:~p~n~p", 
                                        [ProviderModule, Type, Error, Stack]
                                    ),
                                    {[{error, {init_failed, ProviderModule, Error}} | ResultsAcc], StateAcc}
                            end;
                        {error, Reason} ->
                            logger:error("加载运力提供商模块失败 ~p: ~p", [ProviderModule, Reason]),
                            {[{error, {load_failed, ProviderModule, Reason}} | ResultsAcc], StateAcc}
                    end
                end,
                {[], State},
                ProviderFiles
            ),
            
            % 返回结果
            {{ok, Results}, NewState};
        {error, Reason} ->
            % 无法读取提供商目录
            logger:error("无法读取提供商目录: ~p", [Reason]),
            {{error, {read_dir_failed, Reason}}, State}
    end.

%% @doc 根据PID移除提供商
remove_provider_by_pid(Pid, State) ->
    % 查找与PID匹配的提供商代码
    ProviderList = maps:to_list(State),
    case lists:keyfind(Pid, 2, ProviderList) of
        {ProviderModule, _} ->
            % 移除提供商映射
            maps:remove(ProviderModule, State);
        false ->
            State
    end.

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
