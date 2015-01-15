-module(action_info).

-export([date_info/2, time_info/2, distance_info/3]).

-spec date_info(binary(), {integer(), integer()}) -> [{atom(), term()}].
date_info(Action, {DateFrom, DateTo}) ->
  {ok, Items} = db:find_all_by(action:table(Action), 
    [{time, '<', DateTo}, {time, '>', DateFrom}]),
lager:error("Items info ~p", [Items]),

  lists:map(
    fun(Item) ->
      get_action_info(Item)
    end,
    Items).


get_action_info(Item) ->
    Title = proplists:get_value(title, Item),
    Amount = proplists:get_value(amount, Item),
    Time = proplists:get_value(time, Item),
    LocationGeometry = proplists:get_value(location, Item),
    Location = case LocationGeometry of
      {point, '2d', Lat, Long, _, _} ->
        [{latitude, Lat},{longitude, Long}];
      _ -> null
    end,
    ClientId = proplists:get_value(client_id, Item),
    {ok, Device} = device:find_by_client_id(ClientId),
    DeviceList = device:to_list(Device), 
    OsName = proplists:get_value(os_name, DeviceList),
    OsVersion = proplists:get_value(os_version, DeviceList),
    AppId = proplists:get_value(app_id, DeviceList), 
    AppVersion = proplists:get_value(app_version, DeviceList),
    Language = proplists:get_value(language, DeviceList),
    UserId = proplists:get_value(user_id, DeviceList),

    {ok, [UseCount]} = db:fetch_column_by({items_added, {call, count, [1]}},
      [{client_id, ClientId}, {title, Title}]),
    {ok, [BuyCount]} = db:fetch_column_by({items_bought, {call, count, [1]}},
      [{client_id, ClientId}, {title, Title}]),

    ListId = proplists:get_value(list_id, Item),
    {ok, SameListItemsAdded} = db:fetch_column_by({items_added, title},
      [{list_id, ListId}, {title, Title}]),
    {ok, SameListItemsBought} = db:fetch_column_by({items_bought, title},
      [{list_id, ListId}, {title, Title}]),

    [
      {item, [
        {title, Title},
        {amount, Amount},
        {time, Time},
        {location, Location}
      ]},
      {device, [
        {client_id, ClientId},
        {os_name, OsName},
        {os_version, OsVersion},
        {app_id, AppId}, 
        {app_version, AppVersion},
        {language, Language},
        {user_id, UserId}
      ]},
      {metadata, [
        {use_count, UseCount},
        {buy_count, BuyCount},
        {added_in_same_list, SameListItemsAdded},
        {bought_in_same_list, SameListItemsBought}
      ]}
    ].


time_info(Action, Interval) ->
% select array_aggr(title) from items_added group by list_id, round(time / 60000);
  {ok, Titles} = db:fetch_multiple_columns_by(
    {action:table(Action), [{{call, array_agg, [title]}, as, titles}]}, [], 
    [{group_by, [list_id, {call, round, [{time, '/', Interval}]}]}]),
  lists:map(
    fun(Item) ->
      proplists:get_value(titles, Item)
    end,
    Titles).

distance_info(Action, Distance, {{MinLat, MinLong}, {MaxLat, MaxLong}}) ->
  GridStep = get_grid_step(Distance),
% select array_aggr(title) from items_added group_by ST_SnapToGrid(location)
  LineString = iolist_to_binary(["LINESTRING(", float_to_list(MinLat), " ", float_to_list(MinLong), ",", float_to_list(MaxLat), " ", float_to_list(MaxLong), ")"]),
  {ok, Titles} = db:fetch_multiple_columns_by(
    {action:table(Action), [{{call, array_agg, [title]}, as, titles}]}, 
    [{location, '&&', {call, 'ST_Envelope', [{cast, LineString, as, geometry}]}}], 
    [{group_by, [{call, 'ST_SnapToGrid', [location, GridStep]}]}]),
  lists:map(
    fun(Item) ->
      proplists:get_value(titles, Item)
    end,
    Titles).

get_grid_step(Distance) ->
  Distance / 111.

