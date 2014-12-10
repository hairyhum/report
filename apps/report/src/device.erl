-module(device).

-export([find_by_client_id/1, create/2, update/2]).

-record(device, {
  client_id,
  os_name,
  os_version,
  app_id,
  app_version,
  language,
  user_id
}).

-type device() :: #device{}.
-type plist() :: [{binary() | atom(), term()}].
-type alist() :: [{atom(), term()}].

-spec client_id(device()) -> binary() | undefined.
client_id(#device{client_id = ClientId}) -> ClientId.

-spec find_by_client_id(binary()) -> {ok, device()} | {error, not_found}.
find_by_client_id(ClientId) ->
  case db:find_one_by(devices, [{client_id, ClientId}]) of
    {ok, Data} -> {ok, from_list(Data)};
    {error, not_found} -> {error, not_found}
  end.

-spec create(plist(), binary()) -> ok | {errros, term()}.
create(Data, ClientId) ->
  ValidData = get_valid_data(Data),
  db:insert(devices, ValidData ++ [{client_id, ClientId}]).

-spec update(plist(), device()) -> ok | {errros, term()}.
update(Data, Device) ->
  ValidData = get_valid_data(Data),
  DeviceData = to_list(Device),
  ResultData = lists2:ukeyunion(1, ValidData, DeviceData),
  db:update({devices, [{client_id, client_id(Device)}]}, ResultData).

-spec get_valid_data(plist()) -> alist().
get_valid_data(Data) ->
  lists:filtermap(
    fun
      ({<<"os_name">>, Val}) -> {true, {os_name, Val}};
      ({<<"os_version">>, Val}) -> {true, {os_version, Val}};
      ({<<"app_id">>, Val}) -> {true, {app_id, Val}};
      ({<<"app_version">>, Val}) -> {true, {app_version, Val}};
      ({<<"language">>, Val}) -> {true, {language, Val}};
      ({<<"user_id">>, Val}) -> {true, {user_id, Val}};
      (_) -> false
    end,
    Data).

-spec from_list(plist()) -> device().
from_list(DeviceData) ->
  {ok, D} = device:from_json(DeviceData),
  D.

-spec to_list(device()) -> plist().
to_list(Device) ->
  device:to_json(Device).