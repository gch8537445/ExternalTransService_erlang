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
-spec get_user_config(binary()) -> {ok, map()} | {error, term()}.
get_user_config(UserId) ->
    % 构建Redis键
    Key = <<"user:", UserId/binary>>,

    % 从Redis获取用户配置
    case redis_client:get(Key) of
        {ok, ConfigJson} ->
            try jsx:decode(ConfigJson, [return_maps]) of
                Config -> {ok, Config}
            catch
                _:_ -> {error, invalid_user_config}
            end;
        {error, not_found} ->
            {error, {user_not_found, UserId}};
        {error, Reason} ->
            {error, Reason}
    end.