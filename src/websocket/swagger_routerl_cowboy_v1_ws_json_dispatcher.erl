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
%%% @doc Websocket dispatcher
%%% @end
-module(swagger_routerl_cowboy_v1_ws_json_dispatcher).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  init/3,
  websocket_handle/3,
  websocket_info/3,
  websocket_init/3
]).


%%% API functions

init({tcp, http}, _Req, _AppCtx) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, AppCtx) ->
  {ok, Req, swagger_routerl_cowboy_ws:get_routectx(AppCtx)}.

websocket_handle({ping, _Ping}, Req, RouteCtx) ->
  {ok, Req, RouteCtx};
websocket_handle({text, EventTxt}, Req, RouteCtx) ->
  % decode event from a websocket client
  Event = jsx:decode(EventTxt, [return_maps]),
  % dispatch the request
  output(swagger_routerl_cowboy_ws:dispatch(Event, Req, RouteCtx)).

websocket_info(_Info, Req, RouteCtx) ->
  {ok, Req, RouteCtx}.

%% Private functions

output({reply, Msg, Req, RouteCtx}) ->
  {reply, {text, jsx:encode(#{result => ok, context => Msg})}, Req, RouteCtx};
output({shutdown, Req, RouteCtx}) ->
  {shutdown, jsx:encode(#{result => error, context => Req}), RouteCtx};
output(Rest) -> Rest.