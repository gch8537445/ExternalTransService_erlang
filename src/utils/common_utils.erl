-module(common_utils).
-export([format_timestamp/1, is_night_time/0, round_price/1]).

%% @doc 格式化时间戳
%% @param Timestamp 时间戳
%% @returns binary() 格式化的时间字符串
format_timestamp(Timestamp) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = 
        calendar:system_time_to_universal_time(Timestamp, second),
    
    list_to_binary(io_lib:format(
        "~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
        [Year, Month, Day, Hour, Minute, Second]
    )).

%% @doc 判断当前是否是夜间时段
%% @returns boolean() 是否是夜