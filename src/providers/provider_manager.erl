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
    get_provider_config/1,
    register_provider_config/2,
    estimate_price/3
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

-record(state, {
    providers = #{} :: #{atom() => pid()},  % 提供商名称到PID的映射
    configs = #{} :: #{atom() => map()}     % 提供商名称到配置的映射
}).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 启动服务器
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc 获取所有运力提供商
-spec get_all_providers() -> [atom()].
get_all_providers() ->
    gen_server:call(?SERVER, get_all_providers).

%% @doc 获取指定运力提供商的PID
-spec get_provider(atom()) -> {ok, pid()} | {error, not_found}.
get_provider(ProviderName) ->
    gen_server:call(?SERVER, {get_provider, ProviderName}).

%% @doc 获取指定运力提供商的配置
-spec get_provider_config(atom()) -> {ok, map()} | {error, not_found}.
get_provider_config(ProviderName) ->
    gen_server:call(?SERVER, {get_provider_config, ProviderName}).

%% @doc 注册运力提供商配置
-spec register_provider_config(atom(), map()) -> ok.
register_provider_config(ProviderName, Config) ->
    gen_server:cast(?SERVER, {register_provider_config, ProviderName, Config}).

%% @doc 调用所有运力提供商的预估价接口
-spec estimate_price(binary(), binary(), binary()) -> 
    {ok, [#{atom() => term()}]} | {error, term()}.
estimate_price(Start, End, UserId) ->
    gen_server:call(?SERVER, {estimate_price, Start, End, UserId}).

%%====================================================================
%% gen_server回调函数
%%====================================================================

%% @doc 初始化服务器
init([]) ->
    % 注册运力提供商启动和停止事件
    ok = net_kernel:monitor_nodes(true),

    % 初始化状态
    State = #state{},

    % 不再需要定时刷新提供商配置，由各提供商自己负责
    {ok, State}.

%% @doc 处理同步调用
handle_call(get_all_providers, _From, State) ->
    % 返回所有提供商名称
    Providers = maps:keys(State#state.providers),
    {reply, Providers, State};

handle_call({get_provider, ProviderName}, _From, State) ->
    % 返回指定提供商的PID
    case maps:find(ProviderName, State#state.providers) of
        {ok, Pid} -> {reply, {ok, Pid}, State};
        error -> {reply, {error, not_found}, State}
    end;

handle_call({get_provider_config, ProviderName}, _From, State) ->
    % 返回指定提供商的配置
    case maps:find(ProviderName, State#state.configs) of
        {ok, Config} -> {reply, {ok, Config}, State};
        error -> {reply, {error, not_found}, State}
    end;

handle_call({estimate_price, Start, End, UserId}, _From, State) ->
    % 并行调用所有提供商的预估价接口
    Providers = maps:to_list(State#state.providers),

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
                % 调用提供商的预估价接口
                case gen_server:call(Pid, {estimate_price, Params}, 5000) of
                    {ok, Result} ->
                        {ProviderName, Result};
                    {error, Reason} ->
                        {ProviderName, {error, Reason}}
                end
            catch
                exit:{timeout, _} ->
                    {ProviderName, {error, timeout}};
                Type:ErrorReason:Stack ->
                    error_logger:error_msg(
                        "Provider ~p estimatie failed: ~p:~p~n~p",
                        [ProviderName, Type, ErrorReason, Stack]
                    ),
                    {ProviderName, {error, internal_error}}
            end
        end,
        Providers
    ),

    % 构建结果
    {reply, {ok, Results}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @doc 处理异步调用
handle_cast({register_provider, ProviderName, Pid}, State) ->
    % 注册提供商
    Providers = maps:put(ProviderName, Pid, State#state.providers),
    {noreply, State#state{providers = Providers}};

handle_cast({unregister_provider, ProviderName}, State) ->
    % 注销提供商
    Providers = maps:remove(ProviderName, State#state.providers),
    Configs = maps:remove(ProviderName, State#state.configs),
    {noreply, State#state{providers = Providers, configs = Configs}};

handle_cast({register_provider_config, ProviderName, Config}, State) ->
    % 注册提供商配置
    Configs = maps:put(ProviderName, Config, State#state.configs),
    {noreply, State#state{configs = Configs}};

handle_cast(_Request, State) ->
    {noreply, State}.

%% @doc 处理信息
handle_info({nodeup, _Node}, State) ->
    % 节点上线，刷新提供商列表
    {noreply, State};

handle_info({nodedown, _Node}, State) ->
    % 节点下线，刷新提供商列表
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

%% 不再需要从Redis加载所有提供商配置，由各提供商自己负责

%% @doc 并行映射函数
%% 并行执行函数F在列表L的每个元素上
-spec pmap(fun((A) -> B), [A]) -> [B].
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
