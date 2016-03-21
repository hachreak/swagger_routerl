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
%%% @doc Test cowboy rest
%%% @end
-module(swagger_routerl_cowboy_ws_dispatcher_tests).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-include_lib("eunit/include/eunit.hrl").

swagger_routerl_cowboy_ws_dispatcher_test_() ->
  {foreach,
    fun start/0,
    fun stop/1,
    [
     fun init/1,
     fun ping/1,
     fun text/1
    ]
  }.

start() -> ok.

stop(_) -> ok.

init(_) ->
  fun() ->
    AppCtx = swagger_routerl_cowboy_ws:build_context(qwerty, bar),
    ?assertEqual(
       {cowboy_websocket, fuu, bar},
       swagger_routerl_cowboy_ws_dispatcher:init(fuu, AppCtx))
  end.

ping(_) ->
  fun() ->
    AppCtx = swagger_routerl_cowboy_ws:build_context(qwerty, bar),
    RouteCtx = swagger_routerl_cowboy_ws:get_routectx(AppCtx),
    ?assertEqual(
       {ok, fuu, RouteCtx},
       swagger_routerl_cowboy_ws_dispatcher:websocket_handle(
         {ping, hello}, fuu, RouteCtx))
  end.

text(_) ->
  fun() ->
    Event = #{<<"test">> => <<"tset">>},
    Json = jsx:encode(Event),
    AppCtx = swagger_routerl_cowboy_ws:build_context(qwerty, bar),
    RouteCtx = swagger_routerl_cowboy_ws:get_routectx(AppCtx),
    meck:new(swagger_routerl_cowboy_ws,
             [no_link, passthrough, no_history, non_strict]),
    meck:expect(swagger_routerl_cowboy_ws, execute, 3,
                fun(MyEvent, fuu, MyRouteCtx) ->
                    ?assertEqual(RouteCtx, MyRouteCtx),
                    ?assertEqual(Event, MyEvent)
                end
               ),
    try
      swagger_routerl_cowboy_ws_dispatcher:websocket_handle(
        {text, Json}, fuu, RouteCtx)
    after
      meck:validate(swagger_routerl_cowboy_ws),
      meck:unload(swagger_routerl_cowboy_ws)
    end
  end.

