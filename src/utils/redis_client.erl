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
    with_retry(fun() ->
        case poolboy:transaction(?POOL, fun(Worker) -> eredis:q(Worker, ["GET", Key]) end) of
            {ok, undefined} -> 
                logger:debug("Redis键不存在 [Key: ~p]", [Key]),
                {error, not_found};
            {ok, Value} -> {ok, Value};
            {error, Reason} -> 
                logger:error("Redis GET操作失败 [Key: ~p]: ~p", [Key, Reason]),
                {error, Reason}
        end
    end).

%% @doc 设置键值
%% 在Redis中设置指定键的值
set(Key, Value) ->
    with_retry(fun() ->
        case poolboy:transaction(?POOL, fun(Worker) -> eredis:q(Worker, ["SET", Key, Value]) end) of
            {ok, Result} -> {ok, Result};
            {error, Reason} -> 
                logger:error("Redis SET操作失败 [Key: ~p]: ~p", [Key, Reason]),
                {error, Reason}
        end
    end).

%% @doc 设置键值并设置过期时间
%% 在Redis中设置指定键的值，并设置过期时间（秒）
setex(Key, Seconds, Value) ->
    with_retry(fun() ->
        case poolboy:transaction(?POOL, fun(Worker) -> 
            eredis:q(Worker, ["SETEX", Key, integer_to_list(Seconds), Value]) 
        end) of
            {ok, Result} -> {ok, Result};
            {error, Reason} -> 
                logger:error("Redis SETEX操作失败 [Key: ~p, Seconds: ~p]: ~p", 
                    [Key, Seconds, Reason]),
                {error, Reason}
        end
    end).

%% @doc 删除键
%% 从Redis中删除指定键
del(Key) ->
    with_retry(fun() ->
        case poolboy:transaction(?POOL, fun(Worker) -> eredis:q(Worker, ["DEL", Key]) end) of
            {ok, Result} -> {ok, Result};
            {error, Reason} -> 
                logger:error("Redis DEL操作失败 [Key: ~p]: ~p", [Key, Reason]),
                {error, Reason}
        end
    end).

%% @doc 检查键是否存在
%% 检查Redis中是否存在指定键
exists(Key) ->
    with_retry(fun() ->
        case poolboy:transaction(?POOL, fun(Worker) -> eredis:q(Worker, ["EXISTS", Key]) end) of
            {ok, Result} -> {ok, Result};
            {error, Reason} -> 
                logger:error("Redis EXISTS操作失败 [Key: ~p]: ~p", [Key, Reason]),
                {error, Reason}
        end
    end).

%% @doc 设置键的过期时间
%% 为Redis中的指定键设置过期时间（秒）
expire(Key, Seconds) ->
    with_retry(fun() ->
        case poolboy:transaction(?POOL, fun(Worker) -> 
            eredis:q(Worker, ["EXPIRE", Key, integer_to_list(Seconds)]) 
        end) of
            {ok, Result} -> {ok, Result};
            {error, Reason} -> 
                logger:error("Redis EXPIRE操作失败 [Key: ~p, Seconds: ~p]: ~p", 
                    [Key, Seconds, Reason]),
                {error, Reason}
        end
    end).

%% @doc 查找匹配的键
%% 在Redis中查找匹配指定模式的键
keys(Pattern) ->
    with_retry(fun() ->
        case poolboy:transaction(?POOL, fun(Worker) -> eredis:q(Worker, ["KEYS", Pattern]) end) of
            {ok, Result} -> {ok, Result};
            {error, Reason} -> 
                logger:error("Redis KEYS操作失败 [Pattern: ~p]: ~p", [Pattern, Reason]),
                {error, Reason}
        end
    end).

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc 执行Redis命令
%% 通过连接池执行Redis命令
execute_command(Command) ->
    with_retry(fun() ->
        poolboy:transaction(?POOL, fun(Worker) -> eredis:q(Worker, Command) end)
    end).

%% @doc 带重试的执行函数
%% 如果函数执行失败，会重试几次
with_retry(Fun) ->
    with_retry(Fun, 3, 500). % 默认重试3次，每次间隔500毫秒

%% @doc 带重试的执行函数
%% 如果函数执行失败，会重试几次，可以指定重试次数和间隔时间
with_retry(Fun, 0, _Sleep) ->
    % 重试次数用完，返回最后一次执行的结果
    try Fun() of
        Result -> Result
    catch
        Type:Reason:Stack ->
            logger:error(
                "Redis operation failed: ~p:~p~n~p", 
                [Type, Reason, Stack]
            ),
            {error, {Type, Reason}}
    end;
with_retry(Fun, Retries, Sleep) ->
    try Fun() of
        {error, no_connection} ->
            % 连接错误，等待一段时间后重试
            timer:sleep(Sleep),
            with_retry(Fun, Retries - 1, Sleep);
        {error, {connection_error, _}} ->
            % 连接错误，等待一段时间后重试
            timer:sleep(Sleep),
            with_retry(Fun, Retries - 1, Sleep);
        Result ->
            % 成功或其他错误，直接返回
            Result
    catch
        Type:Reason:Stack ->
            logger:error(
                "Redis operation failed: ~p:~p~n~p", 
                [Type, Reason, Stack]
            ),
            % 出现异常，等待一段时间后重试
            timer:sleep(Sleep),
            with_retry(Fun, Retries - 1, Sleep)
    end.
