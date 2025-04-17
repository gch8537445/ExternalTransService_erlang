-module(price_estimate_handler).
-include("../include/records.hrl").

-export([init/2, terminate/3]).

%% @doc 处理HTTP请求
%% @param Req Cowboy请求对象
%% @param State 处理器状态
%% @returns {ok, Req, State}
init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    {ok, handle_request(Method, Req0), State}.

%% @doc 处理POST请求 - 预估价接口
%% @private
%% @param Req Cowboy请求对象
%% @returns {ok, ReplyReq}
handle_request(<<"POST">>, Req0) ->
    % 解析请求体JSON
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try
        % 解析JSON请求体
        RequestData = jsx:decode(Body, [return_maps]),
        
        % 提取参数
        StartCoord = maps:get(<<"start">>, RequestData),
        EndCoord = maps:get(<<"end">>, RequestData),
        UserId = maps:get(<<"user_id">>, RequestData),
        
        % 解析坐标
        StartPoint = parse_coordinates(StartCoord),
        EndPoint = parse_coordinates(EndCoord),
        
        % 调用预估价服务
        Result = pricing_service:estimate_prices(StartPoint, EndPoint, UserId),
        
        % 处理结果并返回
        case Result of
            {ok, Estimates} ->
                % 转换结果为可输出的格式
                ResponseData = format_price_estimates(Estimates),
                ResponseJson = jsx:encode(#{
                    <<"code">> => 0,
                    <<"message">> => <<"success">>,
                    <<"data">> => ResponseData
                }),
                cowboy_req:reply(200, #{
                    <<"content-type">> => <<"application/json">>
                }, ResponseJson, Req1);
            
            {error, Reason} ->
                % 处理错误情况
                ErrorJson = jsx:encode(#{
                    <<"code">> => 1,
                    <<"message">> => error_to_binary(Reason),
                    <<"data">> => null
                }),
                cowboy_req:reply(400, #{
                    <<"content-type">> => <<"application/json">>
                }, ErrorJson, Req1)
        end
    catch
        _:Exception ->
            % 处理异常
            ErrorJson = jsx:encode(#{
                <<"code">> => 2,
                <<"message">> => <<"Invalid request format">>,
                <<"data">> => null
            }),
            cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, ErrorJson, Req1)
    end;

% 处理其他HTTP方法
handle_request(_, Req) ->
    cowboy_req:reply(405, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(#{
        <<"code">> => 3,
        <<"message">> => <<"Method not allowed">>,
        <<"data">> => null
    }), Req).

%% @doc 解析坐标字符串为坐标点记录
%% @private
%% @param CoordBin 格式为"latitude,longitude"的二进制字符串
%% @returns #point{} 坐标点记录
parse_coordinates(CoordBin) when is_binary(CoordBin) ->
    % 分割坐标字符串
    [LatBin, LonBin] = binary:split(CoordBin, <<",">>),
    
    % 转换为浮点数
    Lat = binary_to_float(LatBin),
    Lon = binary_to_float(LonBin),
    
    % 创建坐标点记录
    #point{latitude = Lat, longitude = Lon}.

%% @doc 将预估价记录列表格式化为可输出的JSON
%% @private
%% @param Estimates 预估价记录列表
%% @returns [map()] 预估价JSON列表
format_price_estimates(Estimates) ->
    [format_price_estimate(E) || E <- Estimates].

%% @doc 将单个预估价记录格式化为可输出的JSON
%% @private
%% @param Estimate 预估价记录
%% @returns map() 预估价JSON对象
format_price_estimate(#price_estimate{
    vehicle_type = VehicleType,
    provider_name = ProviderName,
    estimate_price = Price,
    distance = Distance,
    duration = Duration,
    calculation = Calculation
}) ->
    #{
        <<"vehicle_type">> => VehicleType,
        <<"provider_name">> => ProviderName,
        <<"estimate_price">> => Price,
        <<"distance">> => Distance,
        <<"duration">> => Duration,
        <<"calculation">> => Calculation
    }.

%% @doc 将错误原因转换为二进制字符串
%% @private
%% @param Reason 错误原因
%% @returns binary() 错误信息
error_to_binary(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
error_to_binary(Reason) when is_binary(Reason) ->
    Reason;
error_to_binary(Reason) ->
    list_to_binary(io_lib:format("~p", [Reason])).

%% @doc 终止处理器
terminate(_Reason, _Req, _State) ->
    ok.