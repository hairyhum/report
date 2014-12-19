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
  GridStep = get_grid_step(Distance),
% select array_aggr(title) from items_added group_by ST_SnapToGrid(location)
  {ok, Titles} = db:fetch_multiple_columns_by(
    {action:table(Action), [{{call, array_agg, [title]}, as, titles}]}, [], 
    [{group_by, [{call, 'ST_SnapToGrid', [location, GridStep]}]}]),
  AssociatingTitles = lists:map(
    fun(Item) ->
      proplists:get_value(titles, Item)
    end,
    Titles),
  req:reply(200, json:format(AssociatingTitles), Req, State).


get_grid_step(Distance) ->
  Distance / 111.