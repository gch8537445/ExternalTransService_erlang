-module(redis_store).
-include("../include/records.hrl").

-export([get_pricing_rules/0, set_pricing_rules/1, get_pricing_rule/1, set_pricing_rule/1]).

%% @doc 从Redis获取所有计费规则
%% @returns {ok, [#pricing_rule{}]} | {error, Reason}
get_pricing_rules() ->
    % 从Redis获取规则ID列表
    case poolboy:transaction(redis_pool, fun(Worker) ->
        eredis:q(Worker, ["SMEMBERS", "pricing_rules"])
    end) of
        {ok, RuleIds} when length(RuleIds) > 0 ->
            % 并行获取所有规则
            Rules = pmap(fun(RuleId) ->
                case get_pricing_rule(binary_to_list(RuleId)) of
                    {ok, Rule} -> Rule;
                    _ -> undefined
                end
            end, RuleIds),
            
            % 过滤无效规则
            ValidRules = [R || R <- Rules, R =/= undefined],
            {ok, ValidRules};
        _ ->
            {error, no_rules_found}
    end.

%% @doc 将所有计费规则保存到Redis
%% @param Rules 计费规则列表
%% @returns ok | {error, Reason}
set_pricing_rules(Rules) ->
    % 并行保存所有规则
    Results = pmap(fun(Rule) ->
        set_pricing_rule(Rule)
    end, Rules),
    
    % 检查是否所有规则都保存成功
    case lists:all(fun(R) -> R =:= ok end, Results) of
        true -> ok;
        false -> {error, some_rules_failed}
    end.

%% @doc 从Redis获取单个计费规则
%% @param Id 规则ID
%% @returns {ok, #pricing_rule{}} | {error, Reason}
get_pricing_rule(Id) when is_integer(Id) ->
    get_pricing_rule(integer_to_list(Id));
get_pricing_rule(Id) ->
    % 从Redis获取规则数据
    case poolboy:transaction(redis_pool, fun(Worker) ->
        eredis:q(Worker, ["HGETALL", "pricing_rule:" ++ Id])
    end) of
        {ok, []} ->
            {error, not_found};
        {ok, RuleData} ->
            % 解析规则数据
            RuleMap = parse_redis_hash(RuleData),
            
            % 构建规则记录
            Rule = #pricing_rule{
                id = binary_to_integer(maps:get(<<"id">>, RuleMap)),
                vehicle_type = binary_to_integer(maps:get(<<"vehicle_type">>, RuleMap)),
                ipath_trans_code = binary_to_integer(maps:get(<<"ipath_trans_code">>, RuleMap)),
                rule_name = maps:get(<<"rule_name">>, RuleMap),
                formula_template = maps:get(<<"formula_template">>, RuleMap),
                description = maps:get(<<"description">>, RuleMap)
            },
            
            % 获取规则项
            case poolboy:transaction(redis_pool, fun(Worker) ->
                eredis:q(Worker, ["SMEMBERS", "pricing_rule_items:" ++ Id])
            end) of
                {ok, ItemIds} ->
                    % 并行获取所有规则项
                    Items = pmap(fun(ItemId) ->
                        get_pricing_rule_item(binary_to_list(ItemId))
                    end, ItemIds),
                    
                    % 过滤无效规则项
                    ValidItems = [I || {ok, I} <- Items],
                    {ok, Rule#pricing_rule{items = ValidItems}};
                _ ->
                    {ok, Rule#pricing_rule{items = []}}
            end;
        Error ->
            Error
    end.

%% @doc 将单个计费规则保存到Redis
%% @param Rule 计费规则
%% @returns ok | {error, Reason}
set_pricing_rule(Rule) ->
    % 规则ID
    Id = integer_to_list(Rule#pricing_rule.id),
    
    % 构建规则数据
    RuleData = [
        "id", integer_to_list(Rule#pricing_rule.id),
        "vehicle_type", integer_to_list(Rule#pricing_rule.vehicle_type),
        "ipath_trans_code", integer_to_list(Rule#pricing_rule.ipath_trans_code),
        "rule_name", Rule#pricing_rule.rule_name,
        "formula_template", Rule#pricing_rule.formula_template,
        "description", Rule#pricing_rule.description
    ],
    
    % 使用事务保证原子性
    poolboy:transaction(redis_pool, fun(Worker) ->
        % 开始事务
        eredis:q(Worker, ["MULTI"]),
        
        % 保存规则数据
        eredis:q(Worker, ["HMSET", "pricing_rule:" ++ Id | RuleData]),
        
        % 将规则ID添加到集合
        eredis:q(Worker, ["SADD", "pricing_rules", Id]),
        
        % 删除旧规则项集合
        eredis:q(Worker, ["DEL", "pricing_rule_items:" ++ Id]),
        
        % 保存规则项
        lists:foreach(fun(Item) ->
            % 保存规则项
            set_pricing_rule_item(Item, Worker),
            
            % 将规则项ID添加到集合
            ItemId = integer_to_list(Item#pricing_rule_item.id),
            eredis:q(Worker, ["SADD", "pricing_rule_items:" ++ Id, ItemId])
        end, Rule#pricing_rule.items),
        
        % 提交事务
        eredis:q(Worker, ["EXEC"])
    end),
    
    ok.

%% @doc 从Redis获取单个计费规则项
%% @private
%% @param Id 规则项ID
%% @returns {ok, #pricing_rule_item{}} | {error, Reason}
get_pricing_rule_item(Id) ->
    % 从Redis获取规则项数据
    case poolboy:transaction(redis_pool, fun(Worker) ->
        eredis:q(Worker, ["HGETALL", "pricing_rule_item:" ++ Id])
    end) of
        {ok, []} ->
            {error, not_found};
        {ok, ItemData} ->
            % 解析规则项数据
            ItemMap = parse_redis_hash(ItemData),
            
            % 构建规则项记录
            Item = #pricing_rule_item{
                id = binary_to_integer(maps:get(<<"id">>, ItemMap)),
                rule_id = binary_to_integer(maps:get(<<"rule_id">>, ItemMap)),
                fee_name = maps:get(<<"fee_name">>, ItemMap),
                fee_value = binary_to_float(maps:get(<<"fee_value">>, ItemMap)),
                description = maps:get(<<"description">>, ItemMap)
            },
            
            % 解析时间
            Item2 = case maps:get(<<"time_start">>, ItemMap, undefined) of
                undefined -> Item;
                TimeStartBin -> Item#pricing_rule_item{time_start = parse_time(TimeStartBin)}
            end,
            
            Item3 = case maps:get(<<"time_end">>, ItemMap, undefined) of
                undefined -> Item2;
                TimeEndBin -> Item2#pricing_rule_item{time_end = parse_time(TimeEndBin)}
            end,
            
            {ok, Item3};
        Error ->
            Error
    end.

%% @doc 将单个计费规则项保存到Redis
%% @private
%% @param Item 计费规则项
%% @param Worker Redis连接
%% @returns ok | {error, Reason}
set_pricing_rule_item(Item, Worker) ->
    % 规则项ID
    Id = integer_to_list(Item#pricing_rule_item.id),
    
    % 构建规则项数据
    ItemData = [
        "id", integer_to_list(Item#pricing_rule_item.id),
        "rule_id", integer_to_list(Item#pricing_rule_item.rule_id),
        "fee_name", Item#pricing_rule_item.fee_name,
        "fee_value", float_to_list(Item#pricing_rule_item.fee_value),
        "description", Item#pricing_rule_item.description
    ],
    
    % 添加时间信息
    ItemDataWithTime = 
        case Item#pricing_rule_item.time_start of
            undefined -> ItemData;
            TimeStart -> ItemData ++ ["time_start", format_time(TimeStart)]
        end,
    
    ItemDataFinal = 
        case Item#pricing_rule_item.time_end of
            undefined -> ItemDataWithTime;
            TimeEnd -> ItemDataWithTime ++ ["time_end", format_time(TimeEnd)]
        end,
    
    % 保存规则项数据
    eredis:q(Worker, ["HMSET", "pricing_rule_item:" ++ Id | ItemDataFinal]),
    
    ok.

%% @doc 解析Redis哈希数据
%% @private
%% @param HashData Redis哈希数据
%% @returns map() 数据映射
parse_redis_hash(HashData) ->
    parse_redis_hash(HashData, #{}).

parse_redis_hash([], Acc) ->
    Acc;
parse_redis_hash([Key, Value | Rest], Acc) ->
    parse_redis_hash(Rest, maps:put(Key, Value, Acc)).

%% @doc 解析时间字符串
%% @private
%% @param TimeBin 时间字符串
%% @returns {Hour, Minute, Second} 时间元组
parse_time(TimeBin) ->
    [HourBin, MinBin, SecBin] = binary:split(TimeBin, <<":">>, [global]),
    {binary_to_integer(HourBin), binary_to_integer(MinBin), binary_to_integer(SecBin)}.

%% @doc 格式化时间元组为字符串
%% @private
%% @param {Hour, Minute, Second} 时间元组
%% @returns string() 时间字符串
format_time({Hour, Minute, Second}) ->
    io_lib:format("~2..0B:~2..0B:~2..0B", [Hour, Minute, Second]).

%% @doc 并行映射函数
%% @private
%% @param Fun 映射函数
%% @param List 列表
%% @returns list() 映射结果列表
pmap(Fun, List) ->
    Self = self(),
    Refs = lists:map(
        fun(Item) ->
            Ref = make_ref(),
            spawn_link(fun() -> Self ! {Ref, Fun(Item)} end),
            Ref
        end,
        List
    ),
    lists:map(
        fun(Ref) ->
            receive
                {Ref, Result} -> Result
            end
        end,
        Refs
    ).