-module(req).
-export([
  reply/3,
  reply/4,
  with_auth/3,
  with_json_body/3,
  get_lang/1,
  get_user/1,
  get_auth/1,
  get_ip/1
]).

-spec with_json_body(Req, term(), fun((term()) -> Resp)) -> Resp
  when Req  :: cowboy_req:req(),
       Resp :: {ok, Req, term()}.
with_json_body(Req, State, Fun) ->
  {ok, Body, _} = cowboy_req:body(Req),
  case json:parse(Body) of
    {incomplete, _} ->  reply(400, <<"\"Invalid json\"">>, Req, State);
    {error,_,_} -> reply(400, <<"\"Invalid json\"">>, Req, State);
    InData ->
      Fun(InData)
  end.

with_auth(Req, State, Fun) ->
  case get_user(Req) of
    not_found -> reply(401, Req, State);
    User -> Fun(User)
  end.

reply(Status, Req, State) when is_integer(Status) ->
  lager:debug("Reply ~p~n Req ~p~n", [Status, Req]),
  {ok, Req1} = cowboy_req:reply(Status, headers(), Req),
  {ok, Req1, State}.
reply(Status, Body, Req, State) when is_integer(Status) ->
  lager:debug("Reply ~p~n Req ~p~n", [Status, Req]),
  {ok, Req1} = cowboy_req:reply(Status, [{<<"content-type">>, <<"application/json; charset=utf-8">>}] ++ headers(), Body, Req),
  {ok, Req1, State}.


headers() ->
  [ {<<"Access-Control-Allow-Origin">>, <<"*">>},
    {<<"Access-Control-Allow-Methods">>, <<"POST, GET, PUT, DELETE, HEAD, OPTIONS">>},
    {<<"Access-Control-Allow-Headers">>, <<"Origin, X-Requested-With, Content-Type, Accept, Authorization">>},
    {<<"Access-Control-Max-Age">>, <<"1000">>} ].

get_auth(Req) ->
  try cowboy_req:parse_header(<<"authorization">>, Req) of
    {ok, {<<"basic">>,{Login, Pin}}, _} ->
      {Login, Pin};
    _ -> undefined
  catch _:Reason ->
    lager:error("Auth error. Reason: ~p", [Reason]),
    undefined
  end.

get_user(Req) ->
  case get_auth(Req) of
    {Login, Pin} -> 
      UserAuth = report_config:get_config(auth, []),
      AuthLogin = proplists:get_value(login, UserAuth, none),
      AuthPin = proplists:get_value(pin, UserAuth, none),
      case {Login, Pin} of
        {AuthLogin, AuthPin} -> {Login, Pin};
        _ -> not_found
      end;
    undefined -> not_found
  end.

-spec get_lang(cowboy_req:req()) -> binary().
get_lang(Req) ->
  {LangHeader, _} = cowboy_req:header(<<"accept-language">>, Req),
  case LangHeader of
    undefined -> default_language();
    Bin when byte_size(Bin) < 2 -> default_language();
    L ->
      binary:part(L, {0,2})
  end.

default_language() -> <<"en">>.

-spec get_ip(cowboy_req:req()) -> binary().
get_ip(Req) ->
  {RealIpHeader, _} = cowboy_req:header(<<"x-real-ip">>, Req),
  case RealIpHeader of
    undefined ->
      {Peer, _} = cowboy_req:peer(Req),
      {Ip, _Port} = Peer,
      list_to_binary(inet_parse:ntoa(Ip));
    Header -> Header
  end.



