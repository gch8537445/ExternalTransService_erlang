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
    get/2,
    post/2,
    post/3,
    request/5
]).

-define(DEFAULT_TIMEOUT, 5000).
-define(DEFAULT_HEADERS, #{
    <<"User-Agent">> => <<"ExternalTransService/1.0">>,
    <<"Accept">> => <<"application/json">>
}).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 发送GET请求
%% 使用默认选项发送GET请求
-spec get(binary()) -> {ok, integer(), binary()} | {error, term()}.
get(Url) ->
    get(Url, #{}).

%% @doc 发送GET请求
%% 使用指定的头部发送GET请求
-spec get(binary(), map()) -> {ok, integer(), binary()} | {error, term()}.
get(Url, Headers) ->
    request(get, Url, Headers, <<>>, []).

%% @doc 发送POST请求
%% 使用默认选项发送POST请求，自动将Body编码为JSON
-spec post(binary(), map() | binary()) -> {ok, integer(), binary()} | {error, term()}.
post(Url, Body) ->
    post(Url, Body, #{}).

%% @doc 发送POST请求
%% 使用指定的头部发送POST请求，自动将Body编码为JSON
-spec post(binary(), map() | binary(), map()) -> {ok, integer(), binary()} | {error, term()}.
post(Url, Body, Headers) ->
    % 准备请求体
    {ContentType, EncodedBody} = encode_body(Body),
    
    % 合并头部
    MergedHeaders = maps:merge(
        #{<<"Content-Type">> => ContentType},
        Headers
    ),
    
    % 发送请求
    request(post, Url, MergedHeaders, EncodedBody, []).

%% @doc 发送HTTP请求
%% 使用hackney库发送HTTP请求
-spec request(atom(), binary(), map(), binary(), list()) -> 
    {ok, integer(), binary()} | {error, term()}.
request(Method, Url, Headers, Body, Options) ->
    % 合并默认头部
    MergedHeaders = maps:to_list(maps:merge(?DEFAULT_HEADERS, Headers)),
    
    % 合并默认选项
    MergedOptions = [{recv_timeout, ?DEFAULT_TIMEOUT}, {follow_redirect, true} | Options],
    
    % 发送请求
    case hackney:request(Method, Url, MergedHeaders, Body, MergedOptions) of
        {ok, StatusCode, _RespHeaders, ClientRef} ->
            % 读取响应体
            case hackney:body(ClientRef) of
                {ok, RespBody} ->
                    {ok, StatusCode, RespBody};
                {error, Reason} ->
                    {error, {body_error, Reason}}
            end;
        {ok, StatusCode, _RespHeaders} ->
            {ok, StatusCode, <<>>};
        {error, Reason} ->
            {error, {request_error, Reason}}
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc 编码请求体
%% 将请求体编码为适当的格式
-spec encode_body(map() | binary()) -> {binary(), binary()}.
encode_body(Body) when is_map(Body) ->
    % 将Map编码为JSON
    {<<"application/json">>, jsx:encode(Body)};
encode_body(Body) when is_binary(Body) ->
    % 二进制数据，假设已经是正确格式
    {<<"application/octet-stream">>, Body};
encode_body(Body) when is_list(Body) ->
    % 列表，转换为二进制
    {<<"text/plain">>, list_to_binary(Body)};
encode_body(_) ->
    % 其他类型，使用空请求体
    {<<"application/octet-stream">>, <<>>}.

%% @doc 解码响应体
%% 尝试将响应体解码为JSON
-spec decode_body(binary(), binary()) -> {ok, map()} | {error, term()}.
decode_body(<<"application/json", _/binary>>, Body) ->
    try jsx:decode(Body, [return_maps]) of
        DecodedBody -> {ok, DecodedBody}
    catch
        _:Reason -> {error, {json_decode_error, Reason}}
    end;
decode_body(_, Body) ->
    % 非JSON响应，返回原始数据
    {ok, Body}.