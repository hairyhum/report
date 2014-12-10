-module(migrations).

-export([migrate/0, up/1, down/1]).

migrate() ->
  Existing = existing_versions(),
  Defined = defined_versions(),
  [ up(Version) || Version <- Defined, not lists:member(Version, Existing) ].

up(Version) ->
  Migration = get_migration(Version),
  Up = proplists:get_value(<<"up">>, Migration),
  db_transactor:transaction(
    fun() ->
      exec_migration(Up),
      db:insert(migrations, [{version, Version}])
    end).

down(Version) ->
  Migration = get_migration(Version),
  Down = proplists:get_value(<<"down">>, Migration),
  db_transactor:transaction(
    fun() ->
      exec_migration(Down),
      db:delete(migrations, [{version, Version}])
    end).


defined_versions() ->
  Folder = report_config:get_config(migration_folder, ""),
  case file:list_dir(Folder) of
    {error, Reason} ->
      error({reading_migration_folder, Reason});
    {ok, Files} ->
      [ lists:nthtail(7, filename:basename(FileName, ".json")) 
        || FileName <- Files, 
        filename:extension(FileName) == ".json" ]
  end.

existing_versions() ->
  {ok, Versions} = db:fetch_column_by({migrations, version}, []),
  [ binary_to_list(Ver) || Ver <- Versions ].

  
get_migration(Version) ->
  Folder = report_config:get_config(migration_folder, ""),
  DesiredFileName = "Version" ++ Version ++ ".json",
  case file:list_dir(Folder) of
    {error, Reason} ->
      error({reading_migration_folder, Reason});
    {ok, Files} ->
      case [ F || F <- Files, F == DesiredFileName ] of
        [] -> error({migration_not_found, Version});
        [F|_] ->
          FileName = Folder ++ "/" ++ F,
          case file:read_file(FileName) of
            {error, Reason} -> error({reading_migration_file, Reason});
            {ok, Data} ->
              json:parse(Data)
          end
      end
  end.

exec_migration(Strings) ->
  lager:error("Migration ~p", [Strings]),
  [ db:equery(String, [], 1000000) || String <- Strings ].