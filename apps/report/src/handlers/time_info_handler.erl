-module(time_info_handler).
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
  {IntervalBin, _} = cowboy_req:binding(interval, Req),
  {Action, _} = cowboy_req:binding(action, Req),
  case {IntervalBin, Action} of
    {undefined,_} -> req:reply(404, Req, State);
    {_,undefined} -> req:reply(404, Req, State);
    _ -> ok
  end,
  Interval = binary_to_integer(IntervalBin),
% select array_aggr(title) from items_added group by list_id, round(time / 60000);
  {ok, Titles} = db:fetch_multiple_columns_by(
    {action:table(Action), [{{call, array_agg, [title]}, as, titles}]}, [], 
    [{group_by, [list_id, {call, round, [{time, '/', Interval}]}]}]),
  AssociatingTitles = lists:map(
    fun(Item) ->
      proplists:get_value(titles, Item)
    end,
    Titles),
  req:reply(200, json:format(AssociatingTitles), Req, State).