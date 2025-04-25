%%%-------------------------------------------------------------------
%%% @doc
%%% 公式解析与计算服务
%%% 负责解析和计算计费规则中的公式
%%% @end
%%%-------------------------------------------------------------------
-module(self_calc_service).

%% API
-export([self_calc_prices/3]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 计算预估价
%% 使用自己的计算方式计算预估价
self_calc_prices(Start, End, UserId) ->
    % 并行执行两个操作：
    % 1. 调用腾讯地图API获取行程距离和行程用时
    % 2. 获取每个车型对应的计费规则的公式

    % 创建进程获取地图数据
    Parent = self(),
    MapRef = make_ref(),
    spawn_link(
        fun() ->
            Result = tencent_map_service:get_distance_duration(Start, End),
            Parent ! {MapRef, Result}
        end
    ),

    % 创建进程获取计费规则
    RulesRef = make_ref(),
    spawn_link(
        fun() ->
            Result = price_rules_service:get_price_rules(UserId),
            Parent ! {RulesRef, Result}
        end
    ),

    % 等待两个进程的结果
    receive
        {MapRef, {ok, MapData}} ->
            % 地图数据获取成功，等待计费规则
            receive
                {RulesRef, {ok, PricingRules}} ->
                    % 计费规则获取成功，计算预估价
                    calculate_all_car_prices(MapData, PricingRules);
                {RulesRef, {error, Reason}} ->
                    % 计费规则获取失败
                    {error, {pricing_rules_error, Reason}}
            after 5000 ->
                % 超时
                {error, pricing_rules_timeout}
            end;
        {MapRef, {error, Reason}} ->
            % 地图数据获取失败
            {error, {map_error, Reason}}
    after 5000 ->
        % 超时
        {error, map_timeout}
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc 计算所有车型的预估价
%% 根据地图数据和计费规则计算所有车型的预估价
calculate_all_car_prices(MapData, PricingRules) ->
    % 提取距离和时间
    Distance = maps:get(distance, MapData),  % 单位：米
    Duration = maps:get(duration, MapData),  % 单位：秒

    % 转换单位
    DistanceKm = Distance / 1000,  % 转换为公里
    DurationMin = Duration / 60,   % 转换为分钟

    % 为每个车型计算价格
    try
        CarPrices = lists:map(
            fun(Rule) ->
                % 提取ipath_trans_code信息
                TransCode = maps:get(<<"ipath_trans_code">>, Rule),
                % 为了向后兼容，仍然提取car_type，但不再作为主要标识符
                CarType = maps:get(<<"car_type">>, Rule, 0),

                % 计算价格
                {ok, Price, Details} = calculate_car_price_by_rule(
                    Rule,
                    #{
                        <<"distance">> => DistanceKm,
                        <<"duration">> => DurationMin
                    }
                ),

                % 构建结果，使用ipath_trans_code作为主要标识符
                #{
                    ipath_trans_code => TransCode,
                    car_type => CarType,  % 为了向后兼容保留
                    price => Price,
                    currency => <<"CNY">>,
                    details => Details,
                    distance => Distance,
                    duration => Duration
                }
            end,
            PricingRules
        ),

        % 构建最终结果
        Result = #{
            prices => CarPrices,
            map_info => #{
                distance => Distance,
                duration => Duration,
                start_location => maps:get(start_location, MapData),
                end_location => maps:get(end_location, MapData)
            }
        },

        {ok, Result}
    catch
        Type:Reason:Stack ->
            logger:error(
                "Price calculation failed: ~p:~p~n~p",
                [Type, Reason, Stack]
            ),
            {error, calculation_failed}
    end.

%% @doc 计算价格
%% 根据计费规则和变量计算价格
calculate_car_price_by_rule(Rule, Variables) ->
    % 提取公式模板
    FormulaTemplate = maps:get(<<"formula_template">>, Rule, <<"">>),

    % 提取费用项
    FeeItems = maps:get(<<"fee_items">>, Rule, []),

    % 构建变量映射
    VariableMap = build_variable_map(FeeItems, Variables),

    % 替换公式中的变量
    try
        % 解析公式
        {Formula, Details} = parse_formula(FormulaTemplate, VariableMap),

        % 计算公式
        Result = calculate_formula(Formula),

        % 返回结果
        {ok, Result, Details}
    catch
        throw:{error, Reason} ->
            {error, Reason};
        Type:Reason:Stack ->
            logger:error(
                "Formula calculation failed: ~p:~p~n~p",
                [Type, Reason, Stack]
            ),
            {error, formula_error}
    end.

%% @doc 构建变量映射
%% 将费用项和其他变量合并成一个映射
build_variable_map(FeeItems, Variables) ->
    % 从费用项构建基本变量映射
    BaseMap = lists:foldl(
        fun(FeeItem, Acc) ->
            % 提取费用名称和值
            FeeName = maps:get(<<"fee_name">>, FeeItem, <<>>),
            FeeValue = maps:get(<<"fee_value">>, FeeItem, <<"0">>),
            
            % 检查是否是夜间费用
            case is_night_fee_applicable(FeeItem) of
                true ->
                    % 将费用值转换为浮点数
                    try binary_to_float(FeeValue) of
                        Value -> maps:put(FeeName, Value, Acc)
                    catch
                        error:badarg ->
                            % 尝试将整数转换为浮点数
                            try list_to_float(binary_to_list(FeeValue) ++ ".0") of
                                Value -> maps:put(FeeName, Value, Acc)
                            catch
                                error:badarg -> maps:put(FeeName, 0.0, Acc)
                            end
                    end;
                false ->
                    % 夜间费用不适用，设置为0
                    maps:put(FeeName, 0.0, Acc)
            end
        end,
        #{},
        FeeItems
    ),
    
    % 合并其他变量
    maps:fold(
        fun(Key, Value, Acc) ->
            % 将二进制键转换为原子
            KeyBin = if
                is_atom(Key) -> atom_to_binary(Key, utf8);
                true -> Key
            end,
            
            % 将值转换为浮点数
            ValueFloat = if
                is_float(Value) -> Value;
                is_integer(Value) -> float(Value);
                is_binary(Value) ->
                    try binary_to_float(Value)
                    catch
                        error:badarg ->
                            try list_to_float(binary_to_list(Value) ++ ".0")
                            catch
                                error:badarg -> 0.0
                            end
                    end;
                true -> 0.0
            end,
            
            maps:put(KeyBin, ValueFloat, Acc)
        end,
        BaseMap,
        Variables
    ).

%% @doc 检查夜间费用是否适用
%% 根据当前时间和费用项的时间范围判断夜间费用是否适用
is_night_fee_applicable(FeeItem) ->
    % 获取费用名称
    FeeName = maps:get(<<"fee_name">>, FeeItem, <<>>),
    
    % 如果不是夜间费用，直接返回true
    case FeeName of
        <<"night_fee">> ->
            % 获取时间范围
            TimeStart = maps:get(<<"time_start">>, FeeItem, null),
            TimeEnd = maps:get(<<"time_end">>, FeeItem, null),
            
            % 如果没有时间范围，直接返回true
            if
                TimeStart =:= null orelse TimeEnd =:= null ->
                    true;
                true ->
                    % 获取当前时间
                    {_, {Hour, _, _}} = calendar:local_time(),
                    
                    % 解析时间范围
                    {StartHour, _} = parse_time(TimeStart),
                    {EndHour, _} = parse_time(TimeEnd),
                    
                    % 判断当前时间是否在范围内
                    if
                        StartHour < EndHour ->
                            % 正常范围，例如22:00-06:00
                            Hour >= StartHour andalso Hour < EndHour;
                        true ->
                            % 跨天范围，例如22:00-06:00
                            Hour >= StartHour orelse Hour < EndHour
                    end
            end;
        _ ->
            true
    end.

%% @doc 解析时间字符串
%% 将时间字符串解析为小时和分钟，支持ISO 8601格式和纯时间格式
parse_time(TimeStr) ->
    % 根据格式选择不同的解析方式
    HourMinSec = case binary:match(TimeStr, <<"T">>) of
        nomatch ->
            % 纯时间格式，如 "23:00:00"
            TimeStr;
        _ ->
            % ISO 8601格式，如 "2025-04-21T23:00:00Z"
            [_, TimePart] = binary:split(TimeStr, <<"T">>),
            case binary:match(TimePart, <<"Z">>) of
                nomatch -> TimePart;
                _ -> [HMinS, _] = binary:split(TimePart, <<"Z">>), HMinS
            end
    end,
    
    % 分割时、分、秒
    [HourBin, MinBin, _] = binary:split(HourMinSec, <<":">>, [global]),
    
    % 转换为整数
    Hour = binary_to_integer(HourBin),
    Min = binary_to_integer(MinBin),
    
    {Hour, Min}.

%% @doc 解析公式
%% 将公式模板中的变量替换为实际值
parse_formula(FormulaTemplate, VariableMap) ->
    % 正则表达式匹配变量
    {ok, Pattern} = re:compile(<<"#\\{([^}]+)\\}">>),
    
    % 查找所有变量
    {match, Matches} = re:run(FormulaTemplate, Pattern, [global, {capture, all, binary}]),
    
    % 替换变量并构建详情
    {Formula, Details} = lists:foldl(
        fun([Full, VarName], {FormAcc, DetailsAcc}) ->
            % 获取变量值
            case maps:find(VarName, VariableMap) of
                {ok, Value} ->
                    % 将值转换为字符串
                    ValueBin = float_to_binary(Value, [{decimals, 2}]),
                    
                    % 替换变量
                    NewFormAcc = binary:replace(FormAcc, Full, ValueBin, [global]),
                    
                    % 添加到详情
                    NewDetailsAcc = maps:put(VarName, Value, DetailsAcc),
                    
                    {NewFormAcc, NewDetailsAcc};
                error ->
                    throw({error, {unknown_variable, VarName}})
            end
        end,
        {FormulaTemplate, #{}},
        Matches
    ),
    
    {Formula, Details}.

%% @doc 计算公式
%% 计算数学公式的结果
calculate_formula(Formula) ->
    % 将公式转换为Erlang表达式
    {ok, Tokens, _} = erl_scan:string(binary_to_list(Formula) ++ "."),
    {ok, Expr} = erl_parse:parse_exprs(Tokens),
    
    % 创建绑定环境
    Bindings = erl_eval:new_bindings(),
    
    % 计算表达式
    {value, Result, _} = erl_eval:exprs(Expr, Bindings),
    
    % 确保结果是浮点数
    if
        is_float(Result) -> Result;
        is_integer(Result) -> float(Result);
        true -> throw({error, invalid_result})
    end.