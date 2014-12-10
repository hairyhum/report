#!/usr/bin/env escript
%%! -setcookie local -name migrations
main(Tasks) ->
  start_distribution(),
  Node = 'report@127.0.0.1',
  case net_adm:ping(Node) of
    pang ->
      io:format("node_is_not_running~n");
    pong ->
      Res = run_tasks(Node, Tasks),
      io:format("~p~n", [Res])
  end.

start_distribution() ->
  net_kernel:start([node(), shortnames]).

run_tasks(Node, ["migrate"]) ->
  rpc:call(Node, migrations, migrate, []);
run_tasks(Node, ["up", Version]) ->
  rpc:call(Node, migrations, up, [Version]);
run_tasks(Node, ["down", Version]) ->
  rpc:call(Node, migrations, down, [Version]).
