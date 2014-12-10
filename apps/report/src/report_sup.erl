
-module(report_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, FrontConfigPid} = report_config:start_link(),
  % Plan scheduling
  CronSup = {ecrn_sup, {ecrn_sup, start_link, []}, permanent, infinity, supervisor, [ecrn_sup]},

  % DB
  DBSizeArgs = report_config:get_config(postgres_pool, [{size, 10}, {max_overflow, 20}]),
  DBWorkerArgs = report_config:get_config(postgres, []),
  DB = {db, {db, start_link, [{DBSizeArgs, DBWorkerArgs}]}, permanent, infinity, supervisor, [db]},

  % Config
  Config = {report_config, {report_config, start_link, []}, permanent, 2000, worker, [report_config]},

  Services = [
    DB,
    Config
  ],

  exit(FrontConfigPid, temp),
  {ok, { {one_for_one, 1000, 10}, Services} }.

