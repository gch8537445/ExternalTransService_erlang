%% 坐标点记录
-record(point, {
    latitude  :: float(),
    longitude :: float()
}).

%% 行程信息记录
-record(route_info, {
    distance  :: float(),   % 路线距离(米)
    duration  :: integer()  % 行程时间(秒)
}).

%% 计费规则记录
-record(pricing_rule, {
    id              :: integer(),
    vehicle_type    :: integer(),  % 车型编号
    ipath_trans_code:: integer(),  % 运力编码
    rule_name       :: binary(),   % 规则名称
    formula_template:: binary(),   % 计费公式模板
    description     :: binary(),   % 规则描述
    items = []      :: list()      % 计费项目
}).

%% 计费项记录
-record(pricing_rule_item, {
    id          :: integer(),
    rule_id     :: integer(),
    fee_name    :: binary(),
    fee_value   :: float(),
    time_start  :: {integer(), integer(), integer()},  % 生效开始时间
    time_end    :: {integer(), integer(), integer()},  % 生效结束时间
    description :: binary()
}).

%% 价格估计结果记录
-record(price_estimate, {
    vehicle_type    :: integer(),    % 车型
    provider_name   :: binary(),     % 服务提供商
    estimate_price  :: float(),      % 预估价格
    distance        :: float(),      % 路线距离(千米)
    duration        :: integer(),    % 预计时长(分钟)
    calculation     :: map()         % 计算明细
}).