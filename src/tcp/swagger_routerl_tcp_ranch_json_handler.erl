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
-module(swagger_routerl_tcp_ranch_json_handler).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-behaviour(ranch_protocol).

-export([
  init/4,
  start_link/4
]).


%%% API functions

start_link(Ref, Socket, Transport, AppCtx) ->
  Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, AppCtx]),
  {ok, Pid}.

init(Ref, Socket, Transport, AppCtx) ->
  ok = ranch:accept_ack(Ref),
  loop(Socket, Transport, AppCtx).

loop(Socket, Transport, AppCtx) ->
  case Transport:recv(Socket, 0, infinity) of
    {ok, EventTxt} ->
      Event = jsx:decode(EventTxt, [return_maps]),
      % dispatch the request
      AppCtxNew = case output(
            swagger_routerl_tcp:dispatch(Event, none, AppCtx), AppCtx) of
        {reply, Msg, AppCtx2} ->
          Transport:send(Socket, Msg),
          AppCtx2;
        _ -> AppCtx
      end,
      loop(Socket, Transport, AppCtxNew);
    _ -> ok = Transport:close(Socket)
  end.

%% Private functions

output({reply, Msg, _Req, RouteCtx}, AppCtx) ->
  MsgEncoded = jsx:encode(#{result => ok, context => Msg}),
  {reply, MsgEncoded, AppCtx#{routectx => RouteCtx}};
output({error, Error, _Req, RouteCtx}, AppCtx) ->
  MsgEncoded = jsx:encode(#{result => error, context => Error}),
  {reply, MsgEncoded, AppCtx#{routectx => RouteCtx}};
output({ok, _Req, RouteCtx}, AppCtx) ->
  {noreply, AppCtx#{routectx => RouteCtx}}.
