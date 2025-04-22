%%%-------------------------------------------------------------------
%%% @doc
%%% 公式解析与计算服务
%%% 负责解析和计算计费规则中的公式
%%% @end
%%%-------------------------------------------------------------------
-module(formula_service).

%% API
-export([calculate_price/2]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 计算价格
%% 根据计费规则和变量计算价格
calculate_price(Rule, Variables) ->
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

%%====================================================================
%% 内部函数
%%====================================================================

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