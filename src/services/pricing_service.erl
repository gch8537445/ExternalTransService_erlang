-module(pricing_service).
-include("../include/records.hrl").

-export([estimate_prices/3, calculate_price/3]).

%% @doc 获取所有运力的预估价
%% @param StartPoint 起点坐标
%% @param EndPoint 终点坐标
%% @param UserId 用户ID
%% @returns {ok, [#price_estimate{}]} | {error, Reason}
estimate_prices(StartPoint, EndPoint, UserId) ->
    try
        % 并行执行两个异步操作：获取路线信息和所有定价规则
        % 使用异步调用，提高系统响应速度
        RouteInfoFuture = get_route_info_async(StartPoint, EndPoint),
        PricingRulesFuture = get_pricing_rules_async(),
        
        % 等待并获取路线信息
        {ok, RouteInfo} = RouteInfoFuture(),
        
        % 等待并获取所有计费规则
        {ok, PricingRules} = PricingRulesFuture(),
        
        % 检查是否使用第三方运力计算
        UseSelfCalculation = true, % 这里可以根据用户ID或配置决定是否使用自己的计算逻辑
        
        % 根据不同计算模式执行预估价计算
        Estimates = 
            if 
                UseSelfCalculation ->
                    % 使用自己的计算逻辑
                    calculate_estimates(RouteInfo, PricingRules);
                true -> 
                    % 使用第三方运力API
                    Provider = provider_service:select_provider(UserId),
                    provider_service:get_price_estimate(Provider, StartPoint, EndPoint)
            end,
        
        {ok, Estimates}
    catch
        Error:Reason:Stacktrace ->
            error_logger:error_msg("Price estimate error: ~p:~p~n~p~n", 
                                  [Error, Reason, Stacktrace]),
            {error, price_calculation_failed}
    end.

%% @doc 异步获取路线信息
%% @private
%% @param StartPoint 起点坐标
%% @param EndPoint 终点坐标
%% @returns function() 返回一个可以获取结果的函数
get_route_info_async(StartPoint, EndPoint) ->
    Self = self(),
    Ref = make_ref(),
    
    spawn_link(fun() ->
        Result = map_service:get_route_info(StartPoint, EndPoint),
        Self ! {Ref, Result}
    end),
    
    fun() ->
        receive
            {Ref, Result} -> Result
        after 5000 ->
            {error, timeout}
        end
    end.

%% @doc 异步获取所有计费规则
%% @private
%% @returns function() 返回一个可以获取结果的函数
get_pricing_rules_async() ->
    Self = self(),
    Ref = make_ref(),
    
    spawn_link(fun() ->
        % 先尝试从Redis获取缓存的规则
        Result = case redis_store:get_pricing_rules() of
            {ok, Rules} when length(Rules) > 0 ->
                {ok, Rules};
            _ ->
                % 如果Redis没有数据，从PostgreSQL获取
                pg_store:get_pricing_rules()
        end,
        Self ! {Ref, Result}
    end),
    
    fun() ->
        receive
            {Ref, Result} -> Result
        after 5000 ->
            {error, timeout}
        end
    end.

%% @doc 计算所有运力的预估价
%% @private
%% @param RouteInfo 路线信息
%% @param PricingRules 计费规则列表
%% @returns [#price_estimate{}] 预估价列表
calculate_estimates(RouteInfo, PricingRules) ->
    % 定义当前时间，用于判断是否适用夜间费
    {_, {Hour, Minute, _}} = calendar:local_time(),
    CurrentTime = {Hour, Minute, 0},
    
    % 将路线距离从米转换为千米
    DistanceKm = RouteInfo#route_info.distance / 1000,
    
    % 将行程时间从秒转换为分钟
    DurationMin = RouteInfo#route_info.duration / 60,
    
    % 并行计算每种车型的预估价
    lists:map(fun(Rule) ->
        % 异步计算每个规则的价格
        calculate_price_async(Rule, DistanceKm, DurationMin, CurrentTime)
    end, PricingRules).

%% @doc 异步计算单个运力的预估价
%% @private
%% @param Rule 计费规则
%% @param DistanceKm 距离(公里)
%% @param DurationMin 时长(分钟)
%% @param CurrentTime 当前时间
%% @returns #price_estimate{} 预估价记录
calculate_price_async(Rule, DistanceKm, DurationMin, CurrentTime) ->
    % 获取提供商名称
    ProviderName = get_provider_name(Rule#pricing_rule.ipath_trans_code),
    
    % 获取计费变量
    Variables = prepare_variables(Rule, DistanceKm, DurationMin, CurrentTime),
    
    % 计算价格
    {Price, Calculation} = calculate_price(Rule#pricing_rule.formula_template, Variables, Rule),
    
    % 创建预估价记录
    #price_estimate{
        vehicle_type = Rule#pricing_rule.vehicle_type,
        provider_name = ProviderName,
        estimate_price = Price,
        distance = DistanceKm,
        duration = DurationMin,
        calculation = Calculation
    }.

%% @doc 根据运力编码获取提供商名称
%% @private
%% @param IpathTransCode 运力编码
%% @returns binary() 提供商名称
get_provider_name(IpathTransCode) ->
    % 取运力编码的前两位表示公司代码
    CompanyCode = IpathTransCode div 1000,
    
    case CompanyCode of
        11 -> <<"曹操出行">>;
        12 -> <<"高德出行">>;
        22 -> <<"高德出行">>;
        32 -> <<"滴滴出行">>;
        _ -> <<"未知提供商">>
    end.

%% @doc 准备计费公式需要的变量
%% @private
%% @param Rule 计费规则
%% @param DistanceKm 距离(公里)
%% @param DurationMin 时长(分钟)
%% @param CurrentTime 当前时间
%% @returns map() 变量映射
prepare_variables(Rule, DistanceKm, DurationMin, CurrentTime) ->
    % 基础变量
    BaseVars = #{
        <<"distance">> => DistanceKm,
        <<"duration">> => DurationMin
    },
    
    % 添加规则中的费用项变量
    RuleItems = Rule#pricing_rule.items,
    
    % 初始化变量Map
    VarsWithFees = lists:foldl(
        fun(Item, Acc) ->
            % 检查时间相关费用项是否适用
            IsTimeApplicable = is_time_fee_applicable(Item, CurrentTime),
            
            FeeValue = case IsTimeApplicable of
                true -> Item#pricing_rule_item.fee_value;
                false -> 
                    case Item#pricing_rule_item.fee_name of
                        <<"night_fee">> -> 0.0; % 不是夜间时段，夜间费为0
                        _ -> Item#pricing_rule_item.fee_value % 其他费用正常计算
                    end
            end,
            
            % 添加到变量集合
            maps:put(Item#pricing_rule_item.fee_name, FeeValue, Acc)
        end,
        BaseVars,
        RuleItems
    ),
    
    VarsWithFees.

%% @doc 判断时间相关费用是否适用
%% @private
%% @param Item 计费项
%% @param CurrentTime 当前时间
%% @returns boolean() 是否适用
is_time_fee_applicable(Item, CurrentTime) ->
    % 如果没有时间限制，则始终适用
    case {Item#pricing_rule_item.time_start, Item#pricing_rule_item.time_end} of
        {undefined, undefined} -> 
            true;
        {StartTime, EndTime} when StartTime =:= undefined orelse EndTime =:= undefined ->
            true;
        {StartTime, EndTime} ->
            % 检查当前时间是否在费用适用时间范围内
            is_time_between(CurrentTime, StartTime, EndTime)
    end.

%% @doc 判断当前时间是否在指定时间范围内
%% @private
%% @param CurrentTime 当前时间
%% @param StartTime 开始时间
%% @param EndTime 结束时间
%% @returns boolean() 是否在范围内
is_time_between(CurrentTime, StartTime, EndTime) ->
    % 转换时间为分钟表示
    {CH, CM, _} = CurrentTime,
    {SH, SM, _} = StartTime,
    {EH, EM, _} = EndTime,
    
    CurrentMinutes = CH * 60 + CM,
    StartMinutes = SH * 60 + SM,
    EndMinutes = EH * 60 + EM,
    
    % 处理跨天的情况
    if
        StartMinutes > EndMinutes ->
            % 跨天情况（如23:00-05:00）
            CurrentMinutes >= StartMinutes orelse CurrentMinutes =< EndMinutes;
        true ->
            % 普通情况
            CurrentMinutes >= StartMinutes andalso CurrentMinutes =< EndMinutes
    end.

%% @doc 计算价格
%% @param FormulaTemplate 计费公式模板
%% @param Variables 变量映射
%% @param Rule 计费规则（用于记录明细）
%% @returns {Price, Calculation} 价格和计算明细
calculate_price(FormulaTemplate, Variables, Rule) ->
    try
        % 解析公式模板，替换变量
        ParsedFormula = formula_parser:parse(FormulaTemplate, Variables),
        
        % 执行计算
        Price = formula_parser:evaluate(ParsedFormula),
        
        % 记录计算明细
        Calculation = #{
            <<"formula">> => FormulaTemplate,
            <<"parsed_formula">> => ParsedFormula,
            <<"variables">> => Variables,
            <<"rule_name">> => Rule#pricing_rule.rule_name,
            <<"result">> => Price
        },
        
        % 返回计算结果和明细
        {Price, Calculation}
    catch
        Error:Reason ->
            % 处理计算错误
            error_logger:error_msg("Formula calculation error: ~p:~p~nFormula: ~p~nVariables: ~p~n", 
                                  [Error, Reason, FormulaTemplate, Variables]),
            {0.0, #{<<"error">> => <<"calculation_failed">>}}
    end.