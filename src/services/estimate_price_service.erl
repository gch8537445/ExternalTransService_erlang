%%%-------------------------------------------------------------------
%%% @doc
%%% 预估价核心服务
%%% 负责计算预估价或调用运力提供商的预估价接口
%%% @end
%%%-------------------------------------------------------------------
-module(estimate_price_service).

%% API
-export([estimate_price/3]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 预估价
%% 根据用户的设置，决定自己算还是调用运力的预估价接口
estimate_price(Start, End, UserId) ->
    % 从Redis获取用户配置
    case user_service:get_user_config(UserId) of
        {ok, UserConfig} ->
            % 检查是否使用自己的计算方式
            case maps:get(<<"estimate_calc_type">>, UserConfig, 0) of
                0 ->
                    % 0, 使用自己的计算方式
                    calculate_price(Start, End, UserId);
                1 ->
                    % 1, 调用运力提供商的预估价接口
                    call_provider_price(Start, End, UserId)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc 计算预估价
%% 使用自己的计算方式计算预估价
calculate_price(Start, End, UserId) ->
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
            Result = pricing_rules_service:get_pricing_rules(UserId),
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
                    calculate_prices(MapData, PricingRules);
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

%% @doc 计算所有车型的预估价
%% 根据地图数据和计费规则计算所有车型的预估价
calculate_prices(MapData, PricingRules) ->
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
                {ok, Price, Details} = formula_service:calculate_price(
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

%% @doc 调用运力提供商的预估价接口
%% 通过provider_manager_service调用所有运力提供商的预估价接口
call_provider_price(Start, End, UserId) ->
    % 调用provider_manager_service获取所有提供商的预估价
    case provider_manager_service:estimate_price(Start, End, UserId) of
        {ok, ProviderResults} ->
            % 构建结果
            Result = #{
                provider_prices => ProviderResults,
                request_info => #{
                    start_location => Start,
                    end_location => End,
                    user_id => UserId
                }
            },
            {ok, Result};
        {error, Reason} ->
            {error, {provider_error, Reason}}
    end.
