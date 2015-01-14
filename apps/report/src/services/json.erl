-module (json).
-include("types.hrl").
-export([parse/1, format/1]).

-type invalid_json() :: {error,invalid_json,term()}.
-type incomplete() :: {incomplete,term()}.
-spec parse(binary()) -> list() | invalid_json() | incomplete().
parse(String) ->
  case jsonx:decode(String, [{format, proplist}]) of
    {error, _, _} = Err ->
      lager:error("Error ~p decoding ~p~n", [Err, String]),
      Err;
    Res -> Res
  end.

-spec format(list() | null | undefined) -> binary().
format(Plist) ->
  jsonx:encode(Plist).
