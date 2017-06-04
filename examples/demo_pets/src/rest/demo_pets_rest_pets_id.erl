%%%-------------------------------------------------------------------
%% @doc demo_pets REST handler
%% @end
%%%-------------------------------------------------------------------

-module(demo_pets_rest_pets_id).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  from_json/2,
  to_json/2
]).

-export([
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    delete_resource/2,
    init/3,
    resource_exists/2,
    rest_init/2
]).

%%%_ * API -------------------------------------------------------------

init(_Transport, _Req, _AppCtx) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, AppCtx) ->
  {[<<"PUT">>, <<"GET">>, <<"DELETE">>], Req, AppCtx}.

rest_init(Req, AppCtx) ->
  {Id, Req2} = cowboy_req:binding(id, Req),
  {ok, Req2, AppCtx#{id => Id}}.

content_types_accepted(Req, AppCtx) ->
  {[{<<"application/json">>, from_json}], Req, AppCtx}.

content_types_provided(Req, AppCtx) ->
  {[{<<"application/json">>, to_json}], Req, AppCtx}.

resource_exists(Req, #{id := Id, db := Db}=AppCtx) ->
  case ets:lookup(Db, Id) of
    [] -> {false, Req, AppCtx};
    _ -> {true, Req, AppCtx}
  end.

delete_resource(Req, #{id := Id, db := Db}=AppCtx) ->
  ets:delete(Db, Id),
  {true, Req, AppCtx}.

from_json(Req, #{id := Id, db := Db}=AppCtx) ->
  {ok, Body, Req2} = cowboy_req:body(Req),
  Input = jsx:decode(Body),
  ets:insert(Db, {Id, Input}),
  {true, Req2, AppCtx}.

to_json(Req, #{id := Id, db := Db}=AppCtx) ->
  Json = jsx:encode(ets:lookup(Db, Id)),
  {Json, Req, AppCtx}.
