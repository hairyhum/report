-module(distance_info_handler).
-behaviour(cowboy_http_handler).

-export ([init/3, handle/2, terminate/3]).

init(_Type, Req, _Opts) ->
  {ok, Req, no_state}.

handle(Req, State) ->
  {Method, _} = cowboy_req:method(Req),
  case Method of
    <<"GET">> -> show_info(Req, State);
    <<"OPTIONS">> -> req:reply(204, Req, State);
    _ -> req:reply(405, Req, State)
  end.

terminate(_,_,_) -> ok.


show_info(Req, State) ->
  {DistanceBin, _} = cowboy_req:binding(distance, Req),
  {Action, _} = cowboy_req:binding(action, Req),
  {QsVals, _} = cowboy_req:qs_vals(Req),
  Square = get_square(QsVals),
  if 
    DistanceBin == undefined; Action == undefined; Square == undefined ->
      req:reply(404, Req, State);
    true -> ok
  end,
  Distance = binary_to_integer(DistanceBin),  
  Titles = action_info:distance_info(Action, Distance, Square),
  req:reply(200, json:format(Titles), Req, State).


get_square(Data) ->
  MinLatBin = proplists:get_value(<<"min_lat">>, Data),
  MinLongBin = proplists:get_value(<<"min_long">>, Data),

  MaxLatBin = proplists:get_value(<<"max_lat">>, Data),
  MaxLongBin = proplists:get_value(<<"max_long">>, Data),

  if 
    MinLatBin == undefined; MaxLatBin == undefined; MinLongBin == undefined; MaxLongBin == undefined ->
      undefined;
    true ->
      MinLat = binary_to_float_safe(MinLatBin),
      MinLong = binary_to_float_safe(MinLongBin),
      MaxLat = binary_to_float_safe(MaxLatBin),
      MaxLong = binary_to_float_safe(MaxLongBin),
      {{MinLat, MinLong}, {MaxLat, MaxLong}}
  end.


binary_to_float_safe(Bin) when is_binary(Bin) ->
  case is_float_binary(Bin) of
    true -> binary_to_float(Bin);
    false -> float(binary_to_integer(Bin))
  end.

is_float_binary(<<".", _Bin/binary>>) -> true;
is_float_binary(<<_:1/binary, Bin/binary>>) -> is_float_binary(Bin);
is_float_binary(<<>>) -> false.

