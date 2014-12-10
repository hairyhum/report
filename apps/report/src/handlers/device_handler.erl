-module(device_handler).
-behaviour(cowboy_http_handler).

-export ([init/3, handle/2, terminate/3]).

init(_Type, Req, _Opts) ->
  {ok, Req, no_state}.

handle(Req, State) ->
  {Method, _} = cowboy_req:method(Req),
  case Method of
    <<"PUT">> -> add_device(Req, State);
    _ -> req:reply(405, Req, State)
  end.

terminate(_,_,_) -> ok.


add_device(Req, State) ->
  req:with_auth(Req, State, 
    fun(_) ->
      req:with_json_body(Req, State, 
        fun(Data) ->
          {ClientId, _} = cowboy_req:binding(client_id, Req), 
          case ClientId of
            undefined ->
              req:reply(404, Req, State);
            _ ->
              Result = case device:find_by_client_id(ClientId) of
                {ok, Device} -> device:update(Data, Device);
                {error, not_found} -> device:create(Data, ClientId)
              end,
              case Result of
                ok -> req:reply(204, Req, State);
                {errors, Err} -> req:reply(422, json:format([{errors, Err}]), Req, State)
              end
          end
        end)
    end).