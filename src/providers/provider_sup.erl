%%%-------------------------------------------------------------------
%%% @doc
%%% 运力提供商监督者
%%% 负责监督运力提供商管理器和各个运力提供商进程
%%% @end
%%%-------------------------------------------------------------------
-module(provider_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_provider/1, stop_provider/1]).

%% 监督者回调函数
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 启动监督者
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc 启动一个运力提供商
%% 动态添加一个运力提供商子进程
-spec start_provider(atom()) -> {ok, pid()} | {error, term()}.
start_provider(ProviderModule) ->
    % 创建子进程规范
    ChildSpec = #{
        id => ProviderModule,
        start => {ProviderModule, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [ProviderModule]
    },
    % 动态添加子进程
    supervisor:start_child(?SERVER, ChildSpec).

%% @doc 停止一个运力提供商
%% 动态移除一个运力提供商子进程
-spec stop_provider(atom()) -> ok | {error, term()}.
stop_provider(ProviderModule) ->
    % 终止并移除子进程
    case supervisor:terminate_child(?SERVER, ProviderModule) of
        ok ->
            supervisor:delete_child(?SERVER, ProviderModule);
        Error ->
            Error
    end.

%%====================================================================
%% 监督者回调函数
%%====================================================================

%% @doc 初始化监督者
%% 定义子进程规范和监督策略
init([]) ->
    % 定义重启策略
    % one_for_one: 如果一个子进程终止，只重启该子进程
    % 最大重启次数: 1000次
    % 在多少秒内: 3600秒（1小时）
    SupFlags = #{strategy => one_for_one, 
                 intensity => 1000, 
                 period => 3600},
    
    % 运力提供商管理器
    ProviderManagerSpec = #{
        id => provider_manager,
        start => {provider_manager, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [provider_manager]
    },
    
    % 定义子进程列表
    ChildSpecs = [ProviderManagerSpec],
    
    % 返回监督者规范
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% 内部函数
%%====================================================================