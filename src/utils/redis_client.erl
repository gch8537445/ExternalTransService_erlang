%%%-------------------------------------------------------------------
%%% @doc
%%% Redis客户端工具
%%% 提供与Redis交互的接口，使用eredis和poolboy实现连接池
%%% @end
%%%-------------------------------------------------------------------
-module(redis_client).

%% API
-export([
    start_link/1,
    get/1,
    set/2,
    setex/3,
    del/1,
    exists/1,
    expire/2,
    keys/1
]).

%% 内部导出函数
-export([init/1]).

-define(POOL, redis_pool).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 启动Redis客户端
%% 创建一个Redis连接
start_link(Args) ->
    eredis:start_link(Args).

%% @doc 初始化函数
%% 用于poolboy工作进程的初始化
init(Args) ->
    start_link(Args).

%% @doc 获取键值
%% 从Redis获取指定键的值
get(Key) ->
    poolboy:transaction(?POOL, fun(Worker) -> eredis:q(Worker, ["GET", Key]) end).

%% @doc 设置键值
%% 在Redis中设置指定键的值
set(Key, Value) ->
    poolboy:transaction(?POOL, fun(Worker) -> eredis:q(Worker, ["SET", Key, Value]) end).

%% @doc 设置键值并设置过期时间
%% 在Redis中设置指定键的值，并设置过期时间（秒）
setex(Key, Seconds, Value) ->
    poolboy:transaction(?POOL, fun(Worker) -> eredis:q(Worker, ["SETEX", Key, integer_to_list(Seconds), Value]) end).

%% @doc 删除键
%% 从Redis中删除指定键
del(Key) -> poolboy:transaction(?POOL, fun(Worker) -> eredis:q(Worker, ["DEL", Key]) end).

%% @doc 检查键是否存在
%% 检查Redis中是否存在指定键
exists(Key) -> poolboy:transaction(?POOL, fun(Worker) -> eredis:q(Worker, ["EXISTS", Key]) end).

%% @doc 设置键的过期时间
%% 为Redis中的指定键设置过期时间（秒）
expire(Key, Seconds) -> poolboy:transaction(?POOL, fun(Worker) -> eredis:q(Worker, ["EXPIRE", Key, integer_to_list(Seconds)]) end).

%% @doc 查找匹配的键
%% 在Redis中查找匹配指定模式的键
keys(Pattern) -> poolboy:transaction(?POOL, fun(Worker) -> eredis:q(Worker, ["KEYS", Pattern]) end).
