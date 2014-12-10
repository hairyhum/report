-module(action).

-export([add_action/4]).

-type blist() :: [{binary(), term()}].
-type alist() :: [{atom(), term()}].

-spec add_action(binary(), binary(), binary(), blist()) -> ok | {errors, term()}.
add_action(ClientId, Action, ItemId, Data) ->
  Table = table(Action),
  ValidData = get_valid_data(Data),
  db:insert(Table, ValidData ++ [{client_id, ClientId}, {item_id, ItemId}]).

get_valid_data(Data) ->
  lists:filtermap(
    fun
      ({<<"title">>, Val}) -> {true, {title, Val}};
      ({<<"amount">>, Val}) -> {true, {amount, Val}};
      ({<<"time">>, Val}) -> {true, {time, Val}};
      ({<<"ip">>, Val}) -> {true, {ip, Val}};
      ({<<"list_id">>, Val}) -> {true, {list_id, Val}};
      ({<<"location">>, Val}) -> {true, {location, {proplists:get_value(<<"latitude">>, Val), proplists:get_value(<<"longitude">>, Val)}}};
      (_) -> false
    end,
    Data).

table(<<"add">>) -> items_added;
table(<<"buy">>) -> items_bought.