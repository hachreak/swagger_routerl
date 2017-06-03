%%% @author Leonardo Rossi <leonardo.rossi@studenti.unipr.it>
%%% @copyright (C) 2016, 2017 Leonardo Rossi
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

-type appctx()   :: swagger_routerl_router:appctx().
-type req()      :: cowboy_req:req().
-type event()    :: swagger_routerl_router:event().

%%% API functions

compile(Prefix, Yaml, RouteCtx, Ctx) ->
  Endpoint = maps:get(endpoint, Ctx, "/websocket"),
  Handler = maps:get(
              handler, Ctx, swagger_routerl_cowboy_v1_ws_json_dispatcher),
  AppCtx = swagger_routerl_router:compile(Prefix, Yaml, RouteCtx),
  [{Endpoint, Handler, AppCtx}].

-spec dispatch(event(), req(), appctx()) ->
    {reply, term(), req(), appctx()}
  | {ok, req(), appctx()}
  | {error, atom(), req(), appctx()}.
dispatch(Event, Req, AppContext) ->
  swagger_routerl_router:dispatch(Event, Req, AppContext).
