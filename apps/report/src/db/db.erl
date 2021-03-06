-module (db).
-behaviour (supervisor).

-export([start_link/1]).
-export([init/1]).
-export([transaction/1]).
-export([
  fetch_column_by/2,
  fetch_column_by/3,
  fetch_multiple_columns_by/2,
  fetch_multiple_columns_by/3,
  fetch_multiple_columns_by/4,
  find_all_by/2,
  find_one_by/2,
  fetch_raw/1,
  fetch_raw/2]).
-export([insert/2, insert/3, insert/4]).
-export([update/2, update/3]).
-export([delete/2, delete/3]).
-export([equery/3]).


-type worker() :: pid().

-type table() :: atom() | {table(), join, table(), on, {column_spec(), comp(), column_spec()}}.
-type column_spec() :: {table(), term()}.
-type multiple_column_spec() :: {table(), [term()]}.

-type filter() :: [filter_cond()].
-type filter_cond() :: {atom(), term()} | {atom(), in, list()} | {atom(), comp(), term()}.
-type comp() :: '>' | '<' | '=' | '<>'.

-type update_filter() :: table() | {table(), filter_cond()} | {table(), filter()}.

-type returns() :: [] | [atom()] | '*'.

-type values() :: [{atom(), term()}].
-type plist() :: [{atom(), term()}].

-export_type([table/0]).

-export_type([column_spec/0, filter/0, returns/0, update_filter/0, values/0, worker/0]).

-define(POOL_NAME, postgres).
-define(TIMEOUT, 5000).

start_link(Args) ->
  supervisor:start_link({local, db}, ?MODULE, Args).

init(Args) ->
  {SizeArgs, WorkerArgs} = Args,
  PoolArgs = [{name, {local, ?POOL_NAME}},
    {worker_module, db_worker}]
    ++ SizeArgs,
  PoolSpec = poolboy:child_spec(?POOL_NAME, PoolArgs, WorkerArgs),
  DBTransactor = {db_transactor, {db_transactor, start_link, []}, permanent, 2000, worker, [db_transactor]},
  {ok, {{one_for_one, 1000, 10}, [PoolSpec, DBTransactor]}}.

-spec transaction(fun((worker()) -> Result)) -> Result when Result::any().
transaction(Fun) ->
  db_transactor:transaction(Fun, ?TIMEOUT).

-spec fetch_column_by(column_spec(), filter()) -> {ok, list()}.
fetch_column_by(ColumnSpec, Filter) ->
  db_transactor:transaction(
    fun(Worker) ->
      fetch_column_by(Worker, ColumnSpec, Filter)
    end,
    ?TIMEOUT).

-spec fetch_multiple_columns_by(multiple_column_spec(), filter()) -> {ok, plist()}.
fetch_multiple_columns_by(ColumnSpec, Filter) ->
  fetch_multiple_columns_by(ColumnSpec, Filter, [], ?TIMEOUT).

-spec fetch_multiple_columns_by(multiple_column_spec(), filter(), plist()) -> {ok, list()}.
fetch_multiple_columns_by(ColumnSpec, Filter, Extra) ->
  fetch_multiple_columns_by(ColumnSpec, Filter, Extra, ?TIMEOUT).

-spec fetch_multiple_columns_by(multiple_column_spec(), filter(), plist(), integer()) -> {ok, list()}.
fetch_multiple_columns_by(ColumnSpec, Filter, Extra, Timeout) ->
  db_transactor:transaction(
    fun(Worker) ->
      fetch_multiple_columns_by(Worker, ColumnSpec, Filter, Extra, Timeout)
    end,
    ?TIMEOUT).

-spec fetch_raw(binary()) -> {ok, plist()}.
fetch_raw(Query) ->
  fetch_raw(Query, ?TIMEOUT).

-spec fetch_raw(binary(), integer()) -> {ok, plist()}.
fetch_raw(Query, Timeout) ->
  db_transactor:transaction(
    fun(Worker) ->
      db_worker:fetch_raw(Worker, Query, Timeout)
    end,
    ?TIMEOUT).

-spec find_all_by(table(), filter()) -> {ok, [plist()]}.
find_all_by(Table, Filter) ->
  db_transactor:transaction(
    fun(Worker) ->
      find_all_by(Worker, Table, Filter)
    end,
    ?TIMEOUT).

-spec find_one_by(table(), filter()) -> {ok, plist()} | {error, not_found}.
find_one_by(Table, Filter) ->
  db_transactor:transaction(
    fun(Worker) ->
      find_one_by(Worker, Table, Filter)
    end,
    ?TIMEOUT).

-spec update(update_filter(), values()) -> ok.
update(Filter, Values) ->
  db_transactor:transaction(
    fun(Worker) ->
      update(Worker, Filter, Values)
    end,
    ?TIMEOUT).

-spec insert(table(), values(), returns()) -> ok | {ok, plist()}.
insert(Table, Values, Returns) ->
  db_transactor:transaction(
    fun(Worker) ->
      insert(Worker, Table, Values, Returns)
    end,
    ?TIMEOUT).

-spec insert(table(), values()) -> ok.
insert(Table, Values) ->
  insert(Table, Values, []).


-spec delete(table(), filter()) -> ok.
delete(Table, Filter) ->
  db_transactor:transaction(
    fun(Worker) ->
      delete(Worker, Table, Filter)
    end,
    ?TIMEOUT).

-spec delete(worker(), table(), filter()) -> ok.
delete(Worker, Table, Filter) ->
  db_worker:delete(Worker, Table, Filter, ?TIMEOUT).

-spec fetch_column_by(worker(), column_spec(), filter()) -> {ok, list()}.
fetch_column_by(Worker, ColumnSpec, Filter) ->
  % {Table, _} = ColumnSpec,
  FilterWithDeleted = Filter,
  db_worker:fetch_column_by(Worker, ColumnSpec, FilterWithDeleted, ?TIMEOUT).

-spec fetch_multiple_columns_by(worker(), multiple_column_spec(), filter(), plist(), integer()) -> {ok, list()}.
fetch_multiple_columns_by(Worker, ColumnSpec, Filter, Extra, Timeout) ->
  % {Table, _} = ColumnSpec,
  FilterWithDeleted = Filter,
  db_worker:fetch_multiple_columns_by(Worker, ColumnSpec, FilterWithDeleted, Extra, Timeout).


-spec find_all_by(worker(), table(), filter()) -> {ok, [plist()]}.
find_all_by(Worker, Table, Filter) ->
  FilterWithDeleted = Filter,
  db_worker:find_all_by(Worker, Table, FilterWithDeleted, ?TIMEOUT).

-spec find_one_by(worker(), table(), filter()) -> {ok, plist()} | {error, not_found}.
find_one_by(Worker, Table, Filter) ->
  FilterWithDeleted = Filter,
  db_worker:find_one_by(Worker, Table, FilterWithDeleted, ?TIMEOUT).

-spec update(worker(), update_filter(), values()) -> ok.
update(Worker, Table, Values) when is_atom(Table) ->
  update(Worker, {Table, []}, Values);
update(Worker, {Table, Filter}, Values) when is_atom(Table), is_tuple(Filter)  ->
  update(Worker, {Table, [Filter]}, Values);
update(Worker, {Table, Filter}, Values) ->
  FilterWithDeleted = Filter,
  db_worker:update(Worker, {Table, FilterWithDeleted}, Values, ?TIMEOUT).

-spec insert(worker(), table(), values(), returns()) -> ok | {ok, plist()}.
insert(Worker, Table, Values, Returns) ->
  db_worker:insert(
    Worker,
    Table,
    Values,
    Returns,
    ?TIMEOUT).

equery(Query, Params, Timeout) ->
  db_transactor:transaction(
    fun(Worker) ->
      db_worker:equery(Worker, Query, Params, Timeout)
    end).
