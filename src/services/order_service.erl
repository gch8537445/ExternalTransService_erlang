%%%-------------------------------------------------------------------
%%% @doc
%%% 订单服务
%%% 处理预估价、创建订单、查询订单、取消订单等业务逻辑
%%% @end
%%%-------------------------------------------------------------------
-module(order_service).

%% API
-export([estimate_price/1, create_order/1]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 预估价
%% 根据用户的设置，决定自己算还是调用运力提供商的预估价接口
estimate_price(Params) ->
    % 从Redis获取用户配置
    UserConfig = user_service:get_user_config(maps:get(<<"user_id">>, Params)),
    Type = maps:get(<<"estimate_calc_type">>, UserConfig, 0),
    case Type of
        0 ->
            % 0, 使用自己的计算方式
            self_calc_service:self_calc_prices(Params);
        1 ->
            % 1, 调用运力提供商的预估价接口
            estimate_price0(Params);
        2 ->
            % TODO
            estimate_price0(Params)
    end.

estimate_price0(Params) ->
    % 获取提供商列表
    case provider_manager_service:get_all_providers() of
        {ok, Providers} when length(Providers) > 0 ->
            % 并行发送请求给所有提供商
            pmap(
                fun(ProviderModule) ->
                    Result = ProviderModule:estimate_price(Params),
                    case Result of
                        {ok, Result} ->
                            {ProviderModule, Result};
                        _ ->
                            Result
                    end
                end,
                Providers
            );
        {ok, []} ->
            {error, no_provider_available};
        Error ->
            Error
    end.

%% @doc 创建订单
%% 并发调用各个子运力对应的create_order方法
create_order(Params) ->
    % 获取提供商列表
    case provider_manager_service:get_all_providers() of
        {ok, Providers} when length(Providers) > 0 ->
            % 并行发送请求给所有提供商
            pmap(
                fun(ProviderModule) ->
                    Result = ProviderModule:create_order(Params),
                    case Result of
                        {ok, Result} ->
                            {ProviderModule, Result};
                        _ ->
                            Result
                    end
                end,
                Providers
            );
        {ok, []} ->
            {error, no_provider_available};
        Error ->
            Error
    end.

%%====================================================================
%% 内部函数
%%====================================================================

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