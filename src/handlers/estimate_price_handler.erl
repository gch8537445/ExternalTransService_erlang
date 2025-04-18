%%%-------------------------------------------------------------------
%%% @doc
%%% 预估价HTTP处理器
%%% 处理预估价HTTP请求，解析请求参数，调用预估价服务，返回结果
%%% @end
%%%-------------------------------------------------------------------
-module(estimate_price_handler).
-behavior(cowboy_handler).

%% Cowboy回调函数
-export([init/2, terminate/3]).

%%====================================================================
%% Cowboy回调函数
%%====================================================================

%% @doc 初始化处理器
%% 处理HTTP请求，解析JSON请求体，调用预估价服务，返回JSON响应
init(Req0, State) ->
    % 只处理POST请求
    case cowboy_req:method(Req0) of
        <<"POST">> ->
            % 读取请求体
            {ok, Body, Req1} = cowboy_req:read_body(Req0),

            % 解析JSON请求体
            try jsx:decode(Body, [return_maps]) of
                Params ->
                    % 提取参数
                    Start = maps:get(<<"start_location">>, Params, undefined),
                    End = maps:get(<<"end_location">>, Params, undefined),
                    UserId = maps:get(<<"user_id">>, Params, undefined),

                    % 验证参数
                    case validate_params(Start, End, UserId) of
                        ok ->
                            % 调用预估价服务
                            case estimate_price_service:estimate_price(Start, End, UserId) of
                                {ok, Result} ->
                                    % 返回成功响应
                                    Req2 = cowboy_req:reply(200, 
                                        #{<<"content-type">> => <<"application/json">>},
                                        jsx:encode(#{
                                            <<"code">> => 0,
                                            <<"message">> => <<"success">>,
                                            <<"data">> => Result
                                        }),
                                        Req1),
                                    {ok, Req2, State};
                                {error, Reason} ->
                                    % 返回错误响应
                                    Req2 = cowboy_req:reply(400, 
                                        #{<<"content-type">> => <<"application/json">>},
                                        jsx:encode(#{
                                            <<"code">> => 1,
                                            <<"message">> => error_to_binary(Reason),
                                            <<"data">> => null
                                        }),
                                        Req1),
                                    {ok, Req2, State}
                            end;
                        {error, InvalidParam} ->
                            % 返回参数错误响应
                            Req2 = cowboy_req:reply(400, 
                                #{<<"content-type">> => <<"application/json">>},
                                jsx:encode(#{
                                    <<"code">> => 1,
                                    <<"message">> => <<"Invalid parameter: ", InvalidParam/binary>>,
                                    <<"data">> => null
                                }),
                                Req1),
                            {ok, Req2, State}
                    end
            catch
                _:_ ->
                    % 返回JSON解析错误响应
                    Req2 = cowboy_req:reply(400, 
                        #{<<"content-type">> => <<"application/json">>},
                        jsx:encode(#{
                            <<"code">> => 1,
                            <<"message">> => <<"Invalid JSON format">>,
                            <<"data">> => null
                        }),
                        Req1),
                    {ok, Req2, State}
            end;
        _ ->
            % 返回方法不允许响应
            Req1 = cowboy_req:reply(405, 
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{
                    <<"code">> => 1,
                    <<"message">> => <<"Method not allowed">>,
                    <<"data">> => null
                }),
                Req0),
            {ok, Req1, State}
    end.

%% @doc 终止处理器
terminate(_Reason, _Req, _State) ->
    ok.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc 验证请求参数
%% 检查起点、终点和用户ID是否有效
-spec validate_params(binary() | undefined, binary() | undefined, binary() | undefined) -> 
    ok | {error, binary()}.
validate_params(undefined, _, _) ->
    {error, <<"start_location">>};
validate_params(_, undefined, _) ->
    {error, <<"end_location">>};
validate_params(_, _, undefined) ->
    {error, <<"user_id">>};
validate_params(Start, End, _UserId) ->
    % 验证坐标格式
    case validate_coordinate(Start) andalso validate_coordinate(End) of
        true -> ok;
        false -> {error, <<"invalid coordinate format">>}
    end.

%% @doc 验证坐标格式
%% 坐标格式应为"纬度,经度"，例如"39.90469,116.40717"
-spec validate_coordinate(binary()) -> boolean().
validate_coordinate(Coordinate) ->
    try
        % 分割坐标
        [LatBin, LngBin] = binary:split(Coordinate, <<",">>),
        % 转换为浮点数
        Lat = binary_to_float(LatBin),
        Lng = binary_to_float(LngBin),
        % 验证范围
        Lat >= -90 andalso Lat =< 90 andalso Lng >= -180 andalso Lng =< 180
    catch
        _:_ -> false
    end.

%% @doc 将错误原因转换为二进制
-spec error_to_binary(atom() | binary() | string() | tuple()) -> binary().
error_to_binary(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
error_to_binary(Reason) when is_list(Reason) ->
    list_to_binary(Reason);
error_to_binary(Reason) when is_binary(Reason) ->
    Reason;
error_to_binary({user_not_found, UserId}) ->
    <<"User not found: ", UserId/binary>>;
error_to_binary(_) ->
    <<"unknown error">>.
