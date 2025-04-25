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
%% 根据用户的设置，决定自己算还是调用运力提供商的预估价接口
estimate_price(Start, End, UserId) ->
    % 从Redis获取用户配置
    case user_service:get_user_config(UserId) of
        {ok, UserConfig} ->
            % 检查是否使用自己的计算方式
            case maps:get(<<"estimate_calc_type">>, UserConfig, 0) of
                0 ->
                    % 0, 使用自己的计算方式
                    self_calc_service:self_calc_prices(Start, End, UserId);
                1 ->
                    % 1, 调用运力提供商的预估价接口
                    provider_api_service:provider_calc_prices(Start, End, UserId)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% 内部函数
%%====================================================================
