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

ping_test() ->
    AppCtx = swagger_routerl_router:build_context(qwerty, bar),
    ?assertEqual(
       {ok, fuu, AppCtx},
       swagger_routerl_cowboy_ws_dispatcher:websocket_handle(
         {ping, hello}, fuu, AppCtx)).

text_test() ->
    Event = #{<<"test">> => <<"tset">>},
    Json = jsx:encode(Event),
    AppCtx = swagger_routerl_router:build_context(qwerty, bar),
    meck:new(swagger_routerl_cowboy_ws,
             [no_link, passthrough, no_history, non_strict]),
    meck:expect(swagger_routerl_cowboy_ws, execute, 3,
                fun(MyEvent, fuu, MyAppCtx) ->
                    ?assertEqual(AppCtx, MyAppCtx),
                    ?assertEqual(Event, MyEvent)
                end
               ),
    try
      swagger_routerl_cowboy_ws_dispatcher:websocket_handle(
        {text, Json}, fuu, AppCtx)
    after
      meck:validate(swagger_routerl_cowboy_ws),
      meck:unload(swagger_routerl_cowboy_ws)
    end.
