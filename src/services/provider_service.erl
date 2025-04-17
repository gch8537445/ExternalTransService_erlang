-module(provider_service).
-behavior(gen_server).
-include("../include/records.hrl").

%% API函数
-export([start_link/0, get_price_estimate/3, select_provider/1, add_provider/2, remove_provider/1]).

%% gen_server回调
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API函数
%% @doc 启动服务
%% @returns {ok, Pid} | {error, Reason}
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 获取价格预估
%% @param Provider 提供商信息
%% @param StartPoint 起点坐标
%% @param EndPoint 终点坐标
%% @returns [#price_estimate{}] 预估价列表
get_price_estimate(Provider, StartPoint, EndPoint) ->
    gen_server:call(?MODULE, {get_price_estimate, Provider, StartPoint, EndPoint}).

%% @doc 为用户选择合适的服务提供商
%% @param UserId 用户ID
%% @returns Provider 提供商信息
select_provider(UserId) ->
    gen_server:call(?MODULE, {select_provider, UserId}).

%% @doc 添加新的服务提供商(热更新支持)
%% @param Name 提供商名称
%% @param Config 提供商配置
%% @returns ok | {error, Reason}
add_provider(Name, Config) ->
    gen_server:call(?MODULE, {add_provider, Name, Config}).

%% @doc 移除服务提供商
%% @param Name 提供商名称
%% @returns ok | {error, Reason}
remove_provider(Name) ->
    gen_server:call(?MODULE, {remove_provider, Name}).

%% gen_server回调
init([]) ->
    % 获取服务提供商配置
    {ok, ProvidersConfig} = application:get_env(external_trans_service, providers),
    
    % 初始化提供商映射
    Providers = lists:foldl(
        fun({Name, Config}, Acc) ->
            % 尝试加载提供商模块
            ProviderModule = get_provider_module(Name),
            maps:put(Name, {ProviderModule, Config}, Acc)
        end,
        #{},
        ProvidersConfig
    ),
    
    {ok, #{providers => Providers}}.

% 处理获取价格预估请求
handle_call({get_price_estimate, {ProviderName, ProviderModule, Config}, StartPoint, EndPoint}, _From, State) ->
    try
        % 调用提供商模块的价格预估功能
        Result = ProviderModule:estimate_price(StartPoint, EndPoint, Config),
        {reply, Result, State}
    catch
        E:R:S ->
            error_logger:error_msg("Provider estimation error: ~p:~p~n~p~n", [E, R, S]),
            {reply, [], State}
    end;

% 处理选择提供商请求
handle_call({select_provider, UserId}, _From, State = #{providers := Providers}) ->
    % 简单实现：固定选择第一个提供商
    % 实际应用中可以基于用户ID或其他规则选择
    case maps:to_list(Providers) of
        [] -> 
            {reply, undefined, State};
        [{Name, {Module, Config}} | _] ->
            {reply, {Name, Module, Config}, State}
    end;

% 处理添加提供商请求(支持热更新)
handle_call({add_provider, Name, Config}, _From, State = #{providers := Providers}) ->
    try
        % 检查提供商模块是否可用
        ProviderModule = get_provider_module(Name),
        
        % 将新提供商添加到映射
        NewProviders = maps:put(Name, {ProviderModule, Config}, Providers),
        
        % 更新状态
        {reply, ok, State#{providers := NewProviders}}
    catch
        E:R:S ->
            error_logger:error_msg("Add provider error: ~p:~p~n~p~n", [E, R, S]),
            {reply, {error, provider_module_not_found}, State}
    end;

% 处理移除提供商请求
handle_call({remove_provider, Name}, _From, State = #{providers := Providers}) ->
    % 从映射中移除提供商
    NewProviders = maps:remove(Name, Providers),
    
    % 更新状态
    {reply, ok, State#{providers := NewProviders}};

% 处理其他请求
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

% 处理异步消息
handle_cast(_Msg, State) ->
    {noreply, State}.

% 处理其他消息
handle_info(_Info, State) ->
    {noreply, State}.

% 终止回调
terminate(_Reason, _State) ->
    ok.

% 代码热更新回调
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc 获取提供商对应的模块
%% @private
%% @param Name 提供商名称
%% @returns module() 提供商模块
get_provider_module(Name) when is_atom(Name) ->
    ModuleName = atom_to_list(Name) ++ "_provider",
    list_to_atom(ModuleName);
get_provider_module(Name) when is_binary(Name) ->
    get_provider_module(binary_to_atom(Name, utf8)).