-module(lists2).

-export([ukeyunion/3, atomize_keys/1]).

-spec ukeyunion(integer(), [{any(), any()}], [{any(), any()}]) -> [{any(), any()}].
ukeyunion(N, List1, List2) ->
  lists:ukeymerge(N, lists:ukeysort(N, List1) , lists:ukeysort(N, List2)).

-spec atomize_keys([{binary(), term()}]) -> [{atom(), term()}].
atomize_keys(List) ->
  [ {binary_to_atom(Key, utf8), Value} || {Key, Value} <- List ].