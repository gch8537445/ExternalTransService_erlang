-module(external_trans_service_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

%% @doc 启动监督者进程
%% @returns {ok, Pid} | {error, Reason}
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc 监督者初始化回调函数
%% @param Args 初始化参数
%% @returns {ok, {SupervisorSpec, ChildSpecs}}
init([]) ->
    % 定义监督策略
    SupFlags = #{
        strategy => one_for_one,   % 一个子进程崩溃不影响其他进程
        intensity => 5,            % 5秒内最多重启5次
        period => 5
    },
    
    % 子进程规范
    RedisPool = #{
        id => redis_pool,
        start => {poolboy, start_link, [get_redis_pool_config()]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [poolboy]
    },
    
    PgPool = #{
        id => pg_pool,
        start => {poolboy, start_link, [get_pg_pool_config()]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [poolboy]
    },
    
    ProviderService = #{
        id => provider_service,
        start => {provider_service, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [provider_service]
    },
    
    ChildSpecs = [RedisPool, PgPool, ProviderService],
    {ok, {SupFlags, ChildSpecs}}.

%% @doc 获取Redis连接池配置
%% @private
%% @returns PoolConfig
get_redis_pool_config() ->
    {ok, RedisConfig} = application:get_env(external_trans_service, redis),
    PoolSize = proplists:get_value(pool_size, RedisConfig),
    Host = proplists:get_value(host, RedisConfig),
    Port = proplists:get_value(port, RedisConfig),
    Database = proplists:get_value(database, RedisConfig),
    Password = proplists:get_value(password, RedisConfig),
    
    [
        {name, {local, redis_pool}},
        {worker_module, eredis},
        {size, PoolSize},
        {max_overflow, PoolSize},
        {args, [Host, Port, Database, Password, 5000]}
    ].

%% @doc 获取PostgreSQL连接池配置
%% @private
%% @returns PoolConfig
get_pg_pool_config() ->
    {ok, PgConfig} = application:get_env(external_trans_service, postgres),
    PoolSize = proplists:get_value(pool_size, PgConfig),
    Host = proplists:get_value(host, PgConfig),
    Port = proplists:get_value(port, PgConfig),
    Username = proplists:get_value(username, PgConfig),
    Password = proplists:get_value(password, PgConfig),
    Database = proplists:get_value(database, PgConfig),
    
    [
        {name, {local, pg_pool}},
        {worker_module, pg_store},
        {size, PoolSize},
        {max_overflow, PoolSize},
        {args, [#{
            host => Host,
            port => Port,
            username => Username,
            password => Password,
            database => Database
        }]}
    ].