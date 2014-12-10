#!/usr/bin/env escript
%%! -setcookie local -name reloader
main(Names) ->
  start_distribution(),
  Node = 'report@127.0.0.1',
  case net_adm:ping(Node) of
    pang ->
      io:format("node_is_not_running~n");
    pong ->
      Res = lists:map(
        fun(NameStr) ->
          Name = list_to_atom(NameStr),
          rpc:call(Node, code, soft_purge, [Name]),
          rpc:call(Node, code, load_file, [Name])
        end,
        Names),
      io:format("~p~n", [Res])
  end.

start_distribution() ->
  net_kernel:start([node(), shortnames]).

