-module(action_info_handler).
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
  {Action, _} = cowboy_req:binding(action, Req),
  {QsVals, _} = cowboy_req:qs_vals(Req),
  DateFrom = case proplists:get_value(<<"date_from">>, QsVals) of
    undefined -> 0;
    DateFromBin when is_binary(DateFromBin) -> binary_to_integer(DateFromBin)
  end,
  DateTo = case proplists:get_value(<<"date_to">>, QsVals) of
    undefined -> 0;
    DateToBin when is_binary(DateToBin) -> binary_to_integer(DateToBin)
  end,
  Info = action_info:date_info(Action, {DateFrom, DateTo}),
  req:reply(200, json:format(Info), Req, State).


