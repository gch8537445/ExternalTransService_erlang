%%%-------------------------------------------------------------------
%%% @doc
%%% 提供商API服务
%%% 负责与各运力提供商进行实际交互，处理价格估算等核心业务逻辑
%%% @end
%%%-------------------------------------------------------------------
-module(provider_api_service).

%% API
-export([
    estimate_price/3
]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 调用所有运力提供商的预估价接口
-spec estimate_price(Start :: binary() | map(), End :: binary() | map(), UserId :: binary()) -> 
    {ok, list()} | {error, term()}.
estimate_price(Start, End, UserId) ->
    % 创建请求参数
    Params = #{
        start_location => Start,
        end_location => End,
        user_id => UserId
    },

    % 获取提供商列表
    case provider_manager_service:get_all_providers() of
        {ok, Providers} when length(Providers) > 0 ->
            % 并行发送请求给所有提供商
            Results = pmap(
                fun(ProviderModule) ->
                    call_provider_estimate(ProviderModule, Params)
                end,
                Providers
            ),
            {ok, Results};
        {ok, []} ->
            {error, no_provider_available};
        Error ->
            Error
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc 调用单个提供商的预估价接口
-spec call_provider_estimate(ProviderModule :: atom(), Params :: map()) -> 
    {atom(), term()} | {atom(), {error, term()}}.
call_provider_estimate(ProviderModule, Params) ->
    try
        % 直接调用提供者模块的estimate_price函数
        case ProviderModule:estimate_price(Params) of
            {ok, Result} ->
                {ProviderModule, Result};
            {error, Reason} ->
                {ProviderModule, {error, Reason}}
        end
    catch
        exit:{timeout, _} ->
            {ProviderModule, {error, timeout}};
        Type:ErrorReason:Stack ->
            logger:error(
                "Provider ~p estimate failed: ~p:~p~n~p",
                [ProviderModule, Type, ErrorReason, Stack]
            ),
            {ProviderModule, {error, internal_error}}
    end.

%% @doc 并行映射函数
%% 并行执行函数F在列表L的每个元素上
-spec pmap(F :: fun((term()) -> term()), L :: list()) -> list().
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

%% @doc 过滤结果
-spec filter_results(Results :: list()) -> list().
filter_results(Results) ->
    % 这里可以实现自定义的结果过滤逻辑
    % 例如去除错误结果，排序等
    Results. 