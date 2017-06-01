%%% @author Leonardo Rossi <leonardo.rossi@studenti.unipr.it>
%%% @copyright (C) 2016 Leonardo Rossi
%%%
%%% This software is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This software is distributed in the hope that it will be useful, but
%%% WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this software; if not, write to the Free Software Foundation,
%%% Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%
%%% @doc Transform the swagger file into a Cowboy WebSocket routing table.
%%% @end
-module(swagger_routerl_cowboy_ws).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  compile/4,
  dispatch/3
]).

-export_type([routectx/0, appctx/0, routes/0, params/0]).

-type routes()   :: list({re:mp(), handler()}).
-type handler()  :: atom().
-type routectx() :: term().
-type appctx()   :: #{routectx => routectx(), routes => routes()}.
-type req()      :: cowboy_req:req().
-type event()    :: map().
-type params()   :: swagger_routerl_utils:params().

%%% API functions

compile(Prefix, Yaml, RouteCtx, Ctx) ->
  Endpoint = maps:get(endpoint, Ctx, "/websocket"),
  Handler = maps:get(handler, Ctx, swagger_routerl_cowboy_v1_ws_dispatcher),
  AppCtx = swagger_routerl_router:compile(Prefix, Yaml, RouteCtx),
  [{Endpoint, Handler, AppCtx}].

-spec dispatch(event(), req(), appctx()) ->
    {ok, req(), appctx()}
  | {ok, req(), appctx(), hibernate}
  | {reply, cow_ws:frame() | [cow_ws:frame()], req(), appctx()}
  | {reply, cow_ws:frame() | [cow_ws:frame()], req(), appctx(), hibernate}
  | {stop, req(), appctx()}.
dispatch(Event, Req, AppContext) ->
  swagger_routerl_router:dispatch(Event, Req, AppContext).
