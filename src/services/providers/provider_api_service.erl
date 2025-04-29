%%%-------------------------------------------------------------------
%%% @doc
%%% 提供商API服务
%%% 负责与各运力提供商进行实际交互，处理价格估算等核心业务逻辑
%%% @end
%%%-------------------------------------------------------------------
-module(provider_api_service).

%% API
-export([
    provider_calc_prices/1
]).

%%====================================================================
%% API 函数
%%====================================================================
%% @doc 计算供应商预估价并格式化结果
provider_calc_prices(Params) ->
    % 调用provider_api_service获取所有提供商的预估价

    % 获取提供商列表
    case provider_manager_service:get_all_providers() of
        {ok, Providers} when length(Providers) > 0 ->
            % 并行发送请求给所有提供商
            ProviderResults = pmap(
                fun(ProviderModule) ->
                    call_provider_estimate(ProviderModule, Params)
                end,
                Providers
            ),
            % 构建结果
            {ok, ProviderResults};
        {ok, []} ->
            {error, no_provider_available};
        Error ->
            Error
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc 调用单个提供商的预估价接口
call_provider_estimate(ProviderModule, Params) ->
    % 直接调用提供者模块的estimate_price函数
    Result = ProviderModule:estimate_price(Params),
    case Result of
        {ok, Result} ->
            {ProviderModule, Result};
        _ ->
            Result
    end.

%% @doc 并行映射函数
%% 并行执行函数F在列表L的每个元素上
pmap(F, L) ->
    Parent = self(),
    Refs = lists:map(
        fun(X) ->
            Ref = make_ref(),
            spawn_link(
                fun() ->
                    Parent ! {Ref, F(X)}
                end
            ),
            Ref
        end,
        L
    ),
    lists:map(
        fun(Ref) ->
            receive
                {Ref, Result} -> Result
            after 5000 ->
                {error, timeout}
            end
        end,
        Refs
    ).