%%% @author Leonardo Rossi <leonardo.rossi@studenti.unipr.it>
%%% @copyright (C) 2017 Leonardo Rossi
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
-module(swagger_routerl_tcp).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  compile/4,
  dispatch/3
]).

-type appctx()   :: swagger_routerl_router:appctx().
-type req()      :: swagger_routerl_router:req().
-type event()    :: swagger_routerl_router:event().

%%% API functions

compile(Prefix, Yaml, RouteCtx, Ctx) ->
  Name = maps:get(name, Ctx, "tcp_driver"),
  % Protocol = maps:get(protocol, Ctx, "tcp"),
  Port = maps:get(port, Ctx, 54355),
  Handler = maps:get(handler, Ctx, swagger_routerl_tcp_ranch_json_handler),
  AppCtx = swagger_routerl_router:compile(Prefix, Yaml, RouteCtx),
  {Name, Port, Handler, AppCtx}.

-spec dispatch(event(), req(), appctx()) ->
    {reply, term(), req(), appctx()}
  | {ok, req(), appctx()}
  | {error, atom(), req(), appctx()}.
dispatch(Event, Req, AppContext) ->
  swagger_routerl_router:dispatch(Event, Req, AppContext).
