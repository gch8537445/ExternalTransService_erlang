%%%-------------------------------------------------------------------
%%% @doc
%%% HTTP客户端工具
%%% 提供与外部HTTP服务交互的接口，使用hackney库实现
%%% @end
%%%-------------------------------------------------------------------
-module(http_client).

%% API
-export([
    get/1,
    post/1,
    get/2,
    post/2,
    post/3,
    request/5
]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 发送GET请求
%% 使用默认选项发送GET请求
get(Url) ->
    request(get, Url, [], <<>>, []).

%% @doc 发送GET请求
%% 使用指定的头部发送GET请求
get(Url, Headers) ->
    request(get, Url, Headers, <<>>, []).

%% @doc 发送POST请求
%% 使用默认选项发送POST请求
post(Url) ->
    request(post, Url, [], <<>>, []).

%% @doc 发送POST请求
%% 使用默认选项发送POST请求，自动将Body编码为JSON
post(Url, Body) ->
    request(post, Url, #{<<"Content-Type">> => <<"application/json">>}, jsx:encode(Body), []).

%% @doc 发送POST请求
%% 使用指定的头部发送POST请求，自动将Body编码为JSON
post(Url, Body, Headers) ->
    % 合并头部
    MergedHeaders = maps:merge(
        #{<<"Content-Type">> => <<"application/json">>},
        Headers
    ),
    % 发送请求
    request(post, Url, MergedHeaders, jsx:encode(Body), []).

%% @doc 发送HTTP请求
%% 使用hackney库发送HTTP请求
request(Method, Url, Headers, Body, Options) ->
    % 获取hackney配置
    HackneyConfig = application:get_all_env(hackney),
    % 合并默认选项和hackney配置
    MergedOptions = HackneyConfig ++ Options,
    % 发送请求
    ResponseBody = hackney:request(Method, Url, Headers, Body, MergedOptions),
    case ResponseBody of
        {ok, _StatusCode, _RespHeaders, ClientRef} ->
            % 读取响应体
            case hackney:body(ClientRef) of
                {ok, Data} ->
                    jsx:decode(Data, [return_maps]);
                _ ->
                    ResponseBody
            end;
        _ -> % Handles {error, Reason} from hackney:request
            ResponseBody
    end.