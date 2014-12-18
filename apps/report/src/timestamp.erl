-module(timestamp).

-export([epoch/0, now/0, normalize_date/1, from_datetime/1]).

-spec epoch() -> number().
epoch() -> 0.0.

-spec now() -> number().
now() ->
  {Meg, Sec, Mic} = os:timestamp(),
  Meg * 1000000 + Sec + (Mic / 1000000).


normalize_date({Date, {H,M,S}}) -> {Date, {H,M,trunc(S)}}.


from_datetime({{_,_,_},{_,_,_}} = DateTime) ->
  BaseDateInt = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
  DateInt = calendar:datetime_to_gregorian_seconds(normalize_date(DateTime)),
  case DateInt - BaseDateInt of
    N when N > 0 -> N;
    _ -> 0
  end.