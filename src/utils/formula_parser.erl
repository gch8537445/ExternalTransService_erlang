-module(formula_parser).
-export([parse/2, evaluate/1]).

%% @doc 解析公式模板，替换变量
%% @param Template 公式模板
%% @param Variables 变量映射
%% @returns binary() 解析后的公式
parse(Template, Variables) when is_binary(Template) ->
    % 先将模板转换为字符串
    TemplateStr = binary_to_list(Template),
    
    % 使用正则表达式查找变量占位符
    {ok, Regex} = re:compile("\\#\\{([^\\}]+)\\}"),
    
    % 替换所有变量
    {Result, _} = re:replace(TemplateStr, Regex, 
        fun([_, VarName]) ->
            % 查找变量值
            VarNameBin = iolist_to_binary(VarName),
            case maps:get(VarNameBin, Variables, undefined) of
                undefined ->
                    % 未找到变量，保持原样
                    "#{" ++ VarName ++ "}";
                Value when is_float(Value) ->
                    % 浮点数变量
                    float_to_list(Value, [{decimals, 2}]);
                Value when is_integer(Value) ->
                    % 整数变量
                    integer_to_list(Value);
                Value when is_binary(Value) ->
                    % 二进制变量
                    binary_to_list(Value);
                Value ->
                    % 其他类型变量
                    io_lib:format("~p", [Value])
            end
        end,
        TemplateStr, [global, {return, list}]),
    
    % 转换回二进制
    list_to_binary(Result).

%% @doc 评估数学表达式
%% @param Formula 要评估的数学表达式
%% @returns float() 计算结果
evaluate(Formula) when is_binary(Formula) ->
    % 将二进制转换为字符串
    FormulaStr = binary_to_list(Formula),
    
    % 移除所有空格
    CleanFormula = re:replace(FormulaStr, "\\s+", "", [global, {return, list}]),
    
    % 词法分析和语法分析
    {ok, Tokens, _} = erl_scan:string(CleanFormula ++ "."),
    {ok, Expr} = erl_parse:parse_exprs(Tokens),
    
    % 执行表达式
    {value, Result, _} = erl_eval:exprs(Expr, []),
    
    % 确保结果是数字
    case is_number(Result) of
        true -> 
            % 四舍五入到两位小数
            round(Result * 100) / 100;
        false ->
            throw({error, non_numeric_result})
    end.