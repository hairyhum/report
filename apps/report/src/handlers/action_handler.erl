-module(action_handler).
-behaviour(cowboy_http_handler).

-export ([init/3, handle/2, terminate/3]).

init(_Type, Req, _Opts) ->
  {ok, Req, no_state}.

handle(Req, State) ->
  {Method, _} = cowboy_req:method(Req),
  case Method of
    <<"POST">> -> add_action(Req, State);
    _ -> req:reply(405, Req, State)
  end.

terminate(_,_,_) -> ok.


add_action(Req, State) ->
  {Action, _} = cowboy_req:binding(action, Req),
  {ClientId, _} = cowboy_req:binding(client_id, Req),
  {ItemId, _} = cowboy_req:binding(itemId, Req),
  if 
    ClientId == undefined; Action == undefined; ItemId == undefined ->
      req:reply(404, Req, State);
    true ->
      req:with_auth(Req, State, fun(_) ->
        req:with_json_body(Req, State, fun(Data) ->
          Ip = req:get_ip(Req),
          Result = action:add_action(ClientId, Action, ItemId, Data ++ [{<<"ip">>, Ip}]),
          case Result of
            ok -> req:reply(204, Req, State);
            {errors, Err} -> req:reply(422, json:format([{errors, Err}]), Req, State)
          end
        end)
      end)
  end.
