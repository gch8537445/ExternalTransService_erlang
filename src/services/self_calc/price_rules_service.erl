%%%-------------------------------------------------------------------
%%% @doc
%%% 计费规则服务
%%% 负责获取和管理计费规则
%%% @end
%%%-------------------------------------------------------------------
-module(price_rules_service).

%% API
-export([get_price_rules/1]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 获取计费规则
%% 根据用户ID获取适用的计费规则
get_price_rules(UserId) ->
    % 从Redis获取用户配置
    UserConfig = user_service:get_user_config(UserId),
    % 获取用户的ipath_trans_code列表
    case maps:find(<<"ipath_trans_codes">>, UserConfig) of
        {ok, TransCodes} when is_list(TransCodes), length(TransCodes) > 0 ->
            % 获取每个ipath_trans_code的计费规则
            get_rules_for_trans_codes(TransCodes);
        {ok, []} ->
            logger:error("用户没有配置任何运力代码 [UserId: ~p]", [UserId]),
            {error, no_trans_codes_configured};
        error ->
            logger:error("用户配置中找不到运力代码列表 [UserId: ~p]", [UserId]),
            {error, no_trans_codes_configured}
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc 获取ipath_trans_code的计费规则
%% 从Redis获取指定ipath_trans_code的计费规则
get_rules_for_trans_codes(TransCodes) ->
    % 获取所有计费规则
    Rules = lists:map(
        fun(TransCode) ->
            BinTransCode = integer_to_binary(TransCode),
            jsx:decode(redis_client:get(<<"pricing_rule_trans:", BinTransCode/binary>>), [return_maps])
        end,
        TransCodes
    ),
    
    % 过滤出有效的规则
    ValidRules = lists:filter(
        fun(Rule) -> not maps:is_key(<<"error">>, Rule) end,
        Rules
    ),
    
    % 如果没有有效规则，返回错误
    case ValidRules of
        [] ->
            {error, no_valid_rules};
        _ ->
            {ok, ValidRules}
    end.