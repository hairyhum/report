-module(report_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start/0]).

start() ->
  lager:start(),
  qdate:start(),
  application:start(crypto),
  application:start(sasl),
  application:start(xmerl),
  application:start(cowboy),
  application:start(report).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  Dispatch = [
    {'_', [],[
      {[<<"action">>, client_id, action, item_id], [], action_handler, []},
      {[<<"device">>, client_id], [], device_handler, []},
      {[<<"info">>, <<"actions">>, action], [], action_info_handler, []},  % GET params 'date_from', 'date_to'
      {[<<"info">>, <<"group">>, <<"time">>, action, interval], [], time_info_handler, []},
      {[<<"info">>, <<"group">>, <<"location">>, action, distance], [], distance_info_handler, []}
    ]}
  ],
  SupStarted = report_sup:start_link(),
  cowboy:start_http(report_listener, 100,
    [{port, 8080}],
    [{env, [{dispatch, Dispatch}]}]),
  file:write_file("pid.pid", os:getpid()),
  SupStarted.

stop(_State) ->
  ok.
