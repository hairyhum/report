-module(distance_info_handler).
-behaviour(cowboy_http_handler).

-export ([init/3, handle/2, terminate/3]).

init(_Type, Req, _Opts) ->
  {ok, Req, no_state}.

handle(Req, State) ->
  {Method, _} = cowboy_req:method(Req),
  case Method of
    <<"GET">> -> show_info(Req, State);
    _ -> req:reply(405, Req, State)
  end.

terminate(_,_,_) -> ok.


show_info(Req, State) ->
  {DistanceBin, _} = cowboy_req:binding(distance, Req),
  {Action, _} = cowboy_req:binding(action, Req),
  case {DistanceBin, Action} of
    {undefined,_} -> req:reply(404, Req, State);
    {_,undefined} -> req:reply(404, Req, State);
    _ -> ok
  end,
  Distance = binary_to_integer(DistanceBin),  
  Titles = action_info:distance_info(Action, Distance),
  req:reply(200, json:format(Titles), Req, State).


