%%%-------------------------------------------------------------------
%% @doc demo_pets public socket (tcp and websocket) API
%% @end
%%%-------------------------------------------------------------------

-module(demo_pets_socket_pets_id).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  delete/4,
  get/4,
  put/4
]).

%%%_ * API -------------------------------------------------------------

put(#{<<"pet">> := Pet}, Req, [Id], AppCtx) ->
  NewAppCtx = AppCtx#{Id => Pet},
  {reply, #{}, Req, NewAppCtx};
put(_, Req, [_Id], AppCtx) ->
  {error, body_notfound, Req, AppCtx}.

get(_Event, Req, [Id], AppCtx) ->
  try
    Value = maps:get(Id, AppCtx),
    {reply, Value, Req, AppCtx}
  catch error:{badkey, _} ->
    {error, pet_notfound, Req, AppCtx}
  end.

delete(_Event, Req, [Id], AppCtx) ->
  NewAppCtx = maps:remove(Id, AppCtx),
  {reply, #{}, Req, NewAppCtx}.
