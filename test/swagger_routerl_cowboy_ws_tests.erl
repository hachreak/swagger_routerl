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
-module(swagger_routerl_cowboy_ws_tests).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-include_lib("eunit/include/eunit.hrl").

compile_test() ->
    File = [
      {"paths", [
        {"/users/{userid}", ok},
        {"/users/{userid}/email", ok},
        {"/my-clients/{clientid}", ok},
        {"/boxes", ok},
        {"/boxes/{boxid}", ok},
        {"/not-exists/pippo", ok}
      ]}
    ],
    [{"/websocket", swagger_routerl_cowboy_v1_ws_json_dispatcher, Appctx}] =
        swagger_routerl_cowboy_ws:compile("ws_", File, fuubar, #{}),
    Event1 = #{
      <<"path">> => <<"/users/pippo/email">>,
      <<"method">> => <<"get">>
    },
    Event2 = #{
      <<"path">> => <<"/users/pippo/email2">>,
      <<"method">> => <<"get">>
    },
    Event3 = #{
      <<"path">> => <<"/not-exists/pippo">>,
      <<"method">> => <<"get">>
    },

    meck:new(ws_users_userid_email,
             [no_link, passthrough, no_history, non_strict]),
    meck:expect(ws_users_userid_email, get, 4,
                fun(MyEvent, empty, ["pippo"], fuubar) ->
                    ?assertEqual(#{
                       <<"path">> => <<"/users/pippo/email">>,
                       <<"method">> => <<"get">>
                      }, MyEvent),
                    {ok, MyEvent}
                end
               ),
    try
      ?assertEqual({ok, Event1},
                   swagger_routerl_cowboy_ws:dispatch(Event1, empty, Appctx)),
      ?assertEqual({error, endpoint_undefined, empty, fuubar},
                   swagger_routerl_cowboy_ws:dispatch(Event2, empty, Appctx)),
      ?assertEqual({error, handler_undefined, empty, fuubar},
                   swagger_routerl_cowboy_ws:dispatch(Event3, empty, Appctx))
    after
      meck:validate(ws_users_userid_email),
      meck:unload(ws_users_userid_email)
    end,

    Event4 = #{
      <<"path">> => <<"/my-clients/pippo">>,
      <<"method">> => <<"post">>
    },
    meck:new('ws_my_clients_clientid',
             [no_link, passthrough, no_history, non_strict]),
    meck:expect('ws_my_clients_clientid', post, 4,
                fun(MyEvent, empty, ["pippo"], fuubar) ->
                    ?assertEqual(#{
                       <<"path">> => <<"/my-clients/pippo">>,
                       <<"method">> => <<"post">>
                      }, MyEvent),
                    {ok, MyEvent}
                end
               ),
    try
      ?assertEqual({ok, Event4},
                   swagger_routerl_cowboy_ws:dispatch(Event4, empty, Appctx))
    after
      meck:validate('ws_my_clients_clientid'),
      meck:unload('ws_my_clients_clientid')
    end.
