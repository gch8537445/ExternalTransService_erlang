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
    case user_service:get_user_config(UserId) of
        {ok, UserConfig} ->
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
            end;
        {error, Reason} ->
            logger:error("获取计费规则失败, 无法获取用户配置 [UserId: ~p]: ~p", [UserId, Reason]),
            {error, Reason}
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
            % 获取计费规则
            case get_rule_by_trans_code(integer_to_binary(TransCode)) of
                {ok, Rule} -> 
                    Rule;
                {error, Reason} -> 
                    % 如果获取失败，记录错误并返回空规则
                    logger:error("获取运力代码计费规则失败 [TransCode: ~p]: ~p", [TransCode, Reason]),
                    #{<<"ipath_trans_code">> => TransCode, <<"error">> => atom_to_binary(Reason, utf8)}
            end
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

%% @doc 根据ipath_trans_code获取计费规则
%% 从Redis获取指定ipath_trans_code的计费规则
get_rule_by_trans_code(TransCode) ->
    % 构建Redis键
    Key = <<"pricing_rule_trans:", TransCode/binary>>,

    % 从Redis获取计费规则
    case redis_client:get(Key) of
        {ok, RuleJson} ->
            Rule = jsx:decode(RuleJson, [return_maps]),
            {ok, Rule};
        {error, not_found} ->
            % 如果找不到规则，直接返回错误
            logger:error("找不到计费规则 [TransCode: ~p]", [TransCode]),
            {error, {rule_not_found, TransCode}};
        {error, Reason} ->
            logger:error("获取计费规则失败 [TransCode: ~p]: ~p", [TransCode, Reason]),
            {error, Reason}
    end.