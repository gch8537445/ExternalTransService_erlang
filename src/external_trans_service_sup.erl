%%%-------------------------------------------------------------------
%%% @doc
%%% 外部运力服务顶级监督者
%%% 负责监督所有其他进程，确保应用程序的稳定性和容错性
%%% @end
%%%-------------------------------------------------------------------
-module(external_trans_service_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% 监督者回调函数
-export([init/1]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 启动监督者
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

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

    % 从应用程序配置中获取Redis连接参数
    {ok, RedisConfig} = application:get_env(external_trans_service, eredis),
    RedisHost = proplists:get_value(host, RedisConfig),
    RedisPort = proplists:get_value(port, RedisConfig),
    RedisDatabase = proplists:get_value(database, RedisConfig),
    RedisPassword = proplists:get_value(password, RedisConfig),
    RedisPoolSize = proplists:get_value(pool_size, RedisConfig),
    RedisPoolMaxOverflow = proplists:get_value(pool_max_overflow, RedisConfig),

    % Redis连接参数
    RedisArgs = [
        {host, RedisHost},
        {port, RedisPort},
        {database, RedisDatabase},
        {password, RedisPassword},
        {reconnect_sleep, 100},  % 重连间隔时间（毫秒）
        {connect_timeout, 5000}  % 连接超时时间（毫秒）
    ],

    % Redis连接池
    RedisPoolSpec = #{
        id => redis_pool,
        start => {poolboy, start_link, [
            [
                {name, {local, redis_pool}},
                {worker_module, redis_client},
                {size, RedisPoolSize},
                {max_overflow, RedisPoolMaxOverflow}
            ],
            RedisArgs  % 传递Redis连接参数
        ]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [poolboy]
    },

    % 运力提供商监督者
    ProviderSupSpec = #{
        id => provider_sup,
        start => {provider_sup, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => supervisor,
        modules => [provider_sup]
    },

    % 定义子进程列表
    ChildSpecs = [
        RedisPoolSpec,
        ProviderSupSpec
    ],

    % 返回监督者规范
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% 内部函数
%%====================================================================
