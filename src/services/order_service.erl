%%%-------------------------------------------------------------------
%%% @doc
%%% 订单服务
%%% 处理预估价、创建订单、查询订单、取消订单等业务逻辑
%%% @end
%%%-------------------------------------------------------------------
-module(order_service).

%% API
-export([create_order/1, estimate_price/1]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 预估价
%% 根据用户的设置，决定自己算还是调用运力提供商的预估价接口
estimate_price(Params) ->
    % 从Redis获取用户配置
    case user_service:get_user_config(maps:get(<<"user_id">>, Params, undefined)) of
        {ok, UserConfig} ->
            % 检查是否使用自己的计算方式
            case maps:get(<<"estimate_calc_type">>, UserConfig, 0) of
                0 ->
                    % 0, 使用自己的计算方式
                    self_calc_service:self_calc_prices(Params);
                1 ->
                    % 1, 调用运力提供商的预估价接口
                    provider_api_service:provider_calc_prices(Params)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 创建订单
%% 并发调用各个子运力对应的create_order方法
create_order(Params) ->
    % 获取提供商列表
    case provider_manager_service:get_all_providers() of
        {ok, Providers} when length(Providers) > 0 ->
            % 并行发送请求给所有提供商
            ProviderResults = pmap(
                fun(ProviderModule) ->
                    call_provider_create_order(ProviderModule, Params)
                end,
                Providers
            ),

            % 处理结果
            process_provider_results(ProviderResults, Params);
        {ok, []} ->
            {error, no_provider_available};
        Error ->
            Error
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc 调用单个提供商的创建订单接口
call_provider_create_order(ProviderModule, Params) ->
    % 直接调用提供者模块的create_order函数
    case ProviderModule:create_order(Params) of
        {ok, Result} ->
            {ok, Result};
        {error, Reason} ->
            {ProviderModule, {error, Reason}};
        {'EXIT', {timeout, _}} ->
            {ProviderModule, {error, timeout}};
        {'EXIT', Reason} ->
            logger:error(
                "Provider ~p create_order failed: ~p",
                [ProviderModule, Reason]
            ),
            {ProviderModule, {error, internal_error}}
    end.

%% @doc 处理提供商结果
process_provider_results(ProviderResults, Params) ->
    % 过滤出成功的结果
    SuccessResults = lists:filter(
        fun({_, Result}) ->
            case Result of
                {ok, _} -> true;
                _ -> false
            end
        end,
        ProviderResults
    ),
    
    % 如果有成功的结果，返回第一个成功的结果
    case SuccessResults of
        [{Provider, {ok, OrderData}} | _] ->
            % 构建订单结果
            OrderResult = #{
                provider => Provider,
                order_data => OrderData,
                request_info => #{
                    params => Params
                }
            },
            {ok, OrderResult};
        [] ->
            % 所有提供商都失败，返回错误
            ErrorDetails = lists:map(
                fun({Provider, {error, Reason}}) ->
                    #{
                        provider => Provider,
                        reason => Reason
                    }
                end,
                ProviderResults
            ),
            {error, #{
                message => <<"所有运力提供商创建订单失败"/utf8>>,
                details => ErrorDetails
            }}
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
            after 10000 ->
                {error, timeout}
            end
        end,
        Refs
    ).


