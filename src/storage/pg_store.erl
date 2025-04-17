-module(pg_store).
-include("../include/records.hrl").
-behavior(poolboy_worker).

-export([start_link/1, get_pricing_rules/0, get_pricing_rule/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API函数
%% @doc 启动连接
%% @param Args 连接参数
%% @returns {ok, Pid} | {error, Reason}
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%% @doc 获取所有计费规则
%% @returns {ok, [#pricing_rule{}]} | {error, Reason}
get_pricing_rules() ->
    poolboy:transaction(pg_pool, fun(Worker) ->
        gen_server:call(Worker, get_pricing_rules)
    end).

%% @doc 获取特定计费规则
%% @param Id 规则ID
%% @returns {ok, #pricing_rule{}} | {error, Reason}
get_pricing_rule(Id) ->
    poolboy:transaction(pg_pool, fun(Worker) ->
        gen_server:call(Worker, {get_pricing_rule, Id})
    end).

%% gen_server回调
init(Args) ->
    process_flag(trap_exit, true),
    Host = maps:get(host, Args),
    Port = maps:get(port, Args),
    Username = maps:get(username, Args),
    Password = maps:get(password, Args),
    Database = maps:get(database, Args),
    
    case epgsql:connect(Host, Username, Password, #{
        port => Port,
        database => Database
    }) of
        {ok, Connection} ->
            {ok, #{connection => Connection}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(get_pricing_rules, _From, State = #{connection := Connection}) ->
    % SQL查询获取所有计费规则
    {ok, _, RulesResult} = epgsql:equery(Connection, 
        "SELECT id, vehicle_type, ipath_trans_code, rule_name, formula_template, description "
        "FROM pricing_rules"),
    
    % 提取规则信息
    Rules = lists:map(fun(Row) ->
        {Id, VehicleType, IpathTransCode, RuleName, FormulaTemplate, Description} = Row,
        
        % 获取规则项
        {ok, _, ItemsResult} = epgsql:equery(Connection, 
            "SELECT id, rule_id, fee_name, fee_value, time_start, time_end, description "
            "FROM pricing_rule_items WHERE rule_id = $1", 
            [Id]),
        
        % 提取规则项信息
        Items = lists:map(fun(ItemRow) ->
            {ItemId, RuleId, FeeName, FeeValue, TimeStart, TimeEnd, ItemDesc} = ItemRow,
            
            % 构建规则项记录
            #pricing_rule_item{
                id = ItemId,
                rule_id = RuleId,
                fee_name = FeeName,
                fee_value = FeeValue,
                time_start = convert_time(TimeStart),
                time_end = convert_time(TimeEnd),
                description = ItemDesc
            }
        end, ItemsResult),
        
        % 构建规则记录
        #pricing_rule{
            id = Id,
            vehicle_type = VehicleType,
            ipath_trans_code = IpathTransCode,
            rule_name = RuleName,
            formula_template = FormulaTemplate,
            description = Description,
            items = Items
        }
    end, RulesResult),
    
    % 缓存规则到Redis
    case redis_store:set_pricing_rules(Rules) of
        ok -> ok;
        {error, _} -> ok  % 缓存失败不影响主流程
    end,
    
    {reply, {ok, Rules}, State};

handle_call({get_pricing_rule, Id}, _From, State = #{connection := Connection}) ->
    % SQL查询获取指定规则
    {ok, _, RulesResult} = epgsql:equery(Connection, 
        "SELECT id, vehicle_type, ipath_trans_code, rule_name, formula_template, description "
        "FROM pricing_rules WHERE id = $1", 
        [Id]),
    
    case RulesResult of
        [] ->
            {reply, {error, not_found}, State};
        [Row] ->
            {Id, VehicleType, IpathTransCode, RuleName, FormulaTemplate, Description} = Row,
            
            % 获取规则项
            {ok, _, ItemsResult} = epgsql:equery(Connection, 
                "SELECT id, rule_id, fee_name, fee_value, time_start, time_end, description "
                "FROM pricing_rule_items WHERE rule_id = $1", 
                [Id]),
            
            % 提取规则项信息
            Items = lists:map(fun(ItemRow) ->
                {ItemId, RuleId, FeeName, FeeValue, TimeStart, TimeEnd, ItemDesc} = ItemRow,
                
                % 构建规则项记录
                #pricing_rule_item{
                    id = ItemId,
                    rule_id = RuleId,
                    fee_name = FeeName,
                    fee_value = FeeValue,
                    time_start = convert_time(TimeStart),
                    time_end = convert_time(TimeEnd),
                    description = ItemDesc
                }
            end, ItemsResult),
            
            % 构建规则记录
            Rule = #pricing_rule{
                id = Id,
                vehicle_type = VehicleType,
                ipath_trans_code = IpathTransCode,
                rule_name = RuleName,
                formula_template = FormulaTemplate,
                description = Description,
                items = Items
            },
            
            {reply, {ok, Rule}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #{connection := Connection}) ->
    epgsql:close(Connection),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc 转换SQL时间类型为Erlang时间元组
%% @private
%% @param SqlTime SQL时间
%% @returns {Hour, Minute, Second} | undefined
convert_time(undefined) ->
    undefined;
convert_time({Hour, Minute, Second}) ->
    {Hour, Minute, Second}.