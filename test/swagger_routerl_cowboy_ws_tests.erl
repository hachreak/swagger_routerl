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

swagger_routerl_cowboy_ws_test_() ->
  {foreach,
    fun start/0,
    fun stop/1,
    [
     fun build_regex/1,
     fun get_filename/1,
     fun compile/1,
     fun build_context/1,
     fun get_routectx/1
    ]
  }.

start() -> ok.

stop(_) -> ok.

build_regex(_) ->
  fun() ->
    MP = swagger_routerl_cowboy_ws:build_regex(
        "/users/{userid}/email"),
    Handler = fuu,
    ?assertEqual({ok, Handler}, swagger_routerl_cowboy_ws:match(
      "/users/abc/email", [{MP, Handler}])),
    ?assertEqual({error, notfound}, swagger_routerl_cowboy_ws:match(
      "/users", [{MP, Handler}])),
    ?assertEqual({error, notfound}, swagger_routerl_cowboy_ws:match(
      "/users/", [{MP, Handler}])),
    ?assertEqual({error, notfound}, swagger_routerl_cowboy_ws:match(
      "/users/abc/fuu", [{MP, Handler}])),
    ?assertEqual({error, notfound}, swagger_routerl_cowboy_ws:match(
      "/users/abc/email/fuu", [{MP, Handler}]))
  end.

get_filename(_) ->
  fun() ->
    Path = swagger_routerl_cowboy_ws:get_filename(
        "/users/{userid}/email"),
    ?assertEqual(
      ws_users_userid_email, Path
    )
  end.

compile(_) ->
  fun() ->
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
    Routes = swagger_routerl_cowboy_ws:compile(File),
    Event1 = #{
      <<"url">> => "/users/pippo/email",
      <<"method">> => <<"get">>
    },
    Event2 = #{
      <<"url">> => "/users/pippo/email2",
      <<"method">> => <<"get">>
    },
    Event3 = #{
      <<"url">> => "/not-exists/pippo",
      <<"method">> => <<"get">>
    },
    Appctx = #{
      routes => Routes,
      routectx => fuubar
    },
    meck:new(ws_users_userid_email,
             [no_link, passthrough, no_history, non_strict]),
    meck:expect(ws_users_userid_email, get, 3,
                fun(MyEvent, empty, fuubar) ->
                    ?assertEqual(#{
                       <<"url">> => "/users/pippo/email",
                       <<"method">> => <<"get">>
                      }, MyEvent),
                    {return, MyEvent}
                end
               ),
    try
      ?assertEqual({return, Event1},
                   swagger_routerl_cowboy_ws:execute(Event1, empty, Appctx)),
      ?assertEqual({error, notfound},
                   swagger_routerl_cowboy_ws:execute(Event2, empty, Appctx)),
      ?assertEqual({error, notdefined},
                   swagger_routerl_cowboy_ws:execute(Event3, empty, Appctx))
    after
      meck:validate(ws_users_userid_email),
      meck:unload(ws_users_userid_email)
    end,

    Event4 = #{
      <<"url">> => "/my-clients/pippo",
      <<"method">> => <<"post">>
    },
    meck:new('ws_my-clients_clientid',
             [no_link, passthrough, no_history, non_strict]),
    meck:expect('ws_my-clients_clientid', post, 3,
                fun(MyEvent, empty, fuubar) ->
                    ?assertEqual(#{
                       <<"url">> => "/my-clients/pippo",
                       <<"method">> => <<"post">>
                      }, MyEvent),
                    {return, MyEvent}
                end
               ),
    try
      ?assertEqual({return, Event4},
                   swagger_routerl_cowboy_ws:execute(Event4, empty, Appctx))
    after
      meck:validate('ws_my-clients_clientid'),
      meck:unload('ws_my-clients_clientid')
    end
  end.

build_context(_) ->
  fun() ->
    ?assertEqual(
      #{routes => fuu, routectx => bar},
      swagger_routerl_cowboy_ws:build_context(fuu, bar))
  end.

get_routectx(_) ->
  fun() ->
    ?assertEqual(
      bar,
      swagger_routerl_cowboy_ws:get_routectx(
        swagger_routerl_cowboy_ws:build_context(fuu, bar)))
  end.