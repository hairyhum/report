%% Copyright
-module(report_config).
-behaviour (gen_server).

-define (TIMEOUT, 2000).


%% API
-export([get_config/2, update/0, load_config/0, print_status/0]).
-export ([handle_call/3, handle_cast/2, init/1, start_link/0, terminate/2, handle_info/2, code_change/3]).

get_config(Key, Default) ->
  Config = gen_server:call(report_config, get),
  proplists:get_value(Key, Config, Default).

load_config() ->
  case file:read_file("etc/config/env") of
    {ok, Env} ->
      EnvName = re:replace(binary:bin_to_list(Env), "\\s+", "", [global,{return,list}]),

      EnvConfigFileName = "etc/config/config-" ++ EnvName ++ ".yml",
      {ok, [[{<<"config">>, EnvConfig}]]} = yaml:load_file(EnvConfigFileName),

      EnvConfigFormatted = lists:map(fun format_config/1, EnvConfig),

      {ok, [[{<<"config">>, CommonConfig}]]} = yaml:load_file("etc/config/config.yml"),
      CommonConfigFormatted = lists:map(fun format_config/1, CommonConfig),

      DictConfig = recur_merge(recur_from_list(CommonConfigFormatted), recur_from_list(EnvConfigFormatted)),

      Config = recur_from_dict(DictConfig),

      {ok, Config};
    Error -> Error
  end.

%%%%%
format_config({Key, Value}) ->
  {binary_to_atom(Key, utf8),
    if is_list(Value) ->
         lists:map(fun format_config/1, Value);
       true -> Value
    end};
format_config(Value) ->
  Value.

recur_merge(Dict1, Dict2) ->
  dict:merge(
  fun(_Key, V1, V2) ->
      if
        is_list(V1) andalso is_list(V2) ->
          [ recur_merge(Val1, Val2) || {Val1, Val2} <- lists:zip(V1, V2) ];
        true ->
          case is_dict(V1) andalso is_dict(V2) of
            true ->
              recur_merge(V1,V2);
            false ->
              V2
          end
      end
    end,
  Dict1, Dict2).

recur_from_list(List) ->
  case is_proplist(List) of
    true -> dict:from_list([{Key, recur_from_list(Val)} || {Key, Val} <- List]);
    false -> List
  end.

recur_from_dict(Dict) ->
  case is_dict(Dict) of
    true -> [{Key, recur_from_dict(Value)} || {Key, Value} <- dict:to_list(Dict)];
    false ->
      if
        is_binary(Dict) -> binary:bin_to_list(Dict);
        true -> Dict
      end
  end.

is_proplist(List) when is_list(List) ->
  lists:all(
    fun({_,_}) -> true;
      (_) -> false
    end,
  List);
is_proplist(_) -> false.

is_dict(DictCand) when is_tuple(DictCand) andalso element(1, DictCand) =:= dict -> true;
is_dict(_NotDict) -> false.
%%%%%

update() ->
  case load_config() of
    {ok, Config} ->
      gen_server:cast(report_config, {update, Config});
    Err -> Err
  end.

start_link() ->
  gen_server:start_link({local, report_config}, ?MODULE, [], []).

init([]) ->
  timer:apply_interval(5000, report_config, print_status, []),
  timer:apply_interval(?TIMEOUT, report_config, update, []),
  {ok, Config} = load_config(),
  {ok, Config}.

handle_call(get, _From, Config) ->
  {reply, Config, Config}.

handle_cast({update, NewConfig}, _OldConfig) ->
  {noreply, NewConfig}.

handle_info(_Message, State) -> {noreply, State}.

terminate(timeout, _State) ->
  ok;
terminate(_,_) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

print_status() ->
  case report_config:get_config(log_status, false) of
    true ->
      lager:debug("Workers: ~p~n", [poolboy:status(postgres)]),
      lager:debug("Memory usage ~p", [erlang:memory()]);
    false ->
      ok
  end.


