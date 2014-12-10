-module(report_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start/0]).

start() ->
  lager:start(),
  qdate:start(),
  inets:start(),
  push_service:start(),
  application:start(crypto),
  application:start(sasl),
  application:start(xmerl),
  application:start(report).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  SupStarted = report_sup:start_link(),
  file:write_file("pid.pid", os:getpid()),
  SupStarted.

stop(_State) ->
  % cowboy:stop_listener(php_listener),
  ok.
% pgsql:connect("localhost", ["postgres"], ["danniill"], [{database, "baton"}]).

% start_http(Ref, NbAcceptors, TransOpts, ProtoOpts)
%     when is_integer(NbAcceptors), NbAcceptors > 0 ->
%   ranch:start_listener(Ref, NbAcceptors,
%     ranch_tcp, TransOpts, baton_protocol, ProtoOpts).