%%%-------------------------------------------------------------------
%%% @doc
%%% 用户服务
%%% 负责获取和管理用户配置信息
%%% @end
%%%-------------------------------------------------------------------
-module(user_service).

%% API
-export([get_user_config/1]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 获取用户配置
%% 从Redis获取用户配置信息
get_user_config(UserId) ->
    % 构建Redis键
    Key = <<"user:", UserId/binary>>,

    % 从Redis获取用户配置
    case redis_client:get(Key) of
        {ok, ConfigJson} ->
            Result = jsx:decode(ConfigJson, [return_maps]),
            {ok, Result};
        {error, not_found} ->
            logger:error("用户配置不存在 [UserId: ~p]", [UserId]),
            {error, {user_not_found, UserId}};
        {error, Reason} ->
            logger:error("获取用户配置失败 [UserId: ~p]: ~p", [UserId, Reason]),
            {error, Reason}
    end.