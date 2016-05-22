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
%%% @doc Test mqtt plugin.
%%% @end
-module(swagger_routerl_emqtt_tests).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
  {foreach,
    fun start/0,
    fun stop/1,
    [
     fun get_filename/1,
     fun build_regex/1,
     fun extract_method/1,
     fun compile/1
    ]
  }.

start() -> ok.

stop(_) -> ok.

get_filename(_) ->
  fun() ->
    ?assertEqual(
      emqtt_users_userid,
      swagger_routerl_emqtt:get_filename("/users/{userid}")
    ),
    ?assertEqual(
      emqtt_my_users_userid,
      swagger_routerl_emqtt:get_filename("/my-users/{userid}")
    )
  end.

build_regex(_) ->
  fun() ->
    Config = [{"get", get_config}, {"post", post_config},
              {"put", put_config}, {"delete", delete_config}],
    [MPDELETE, MPPUT, MPPOST, MPGET] = swagger_routerl_emqtt:build_regex(
        "/users/{userid}/email", Config),
    {match, _} = re:run("/get/users/userid/email", MPGET),
    {match, _} = re:run("/post/users/userid/email", MPPOST),
    {match, _} = re:run("/put/users/userid/email", MPPUT),
    {match, _} = re:run("/delete/users/userid/email", MPDELETE)
  end.

extract_method(_) ->
  fun() ->
    AppCtx = swagger_routerl_emqtt:build_context(routes, routectx),
    ?assertEqual(
       <<"get">>,
       swagger_routerl_emqtt:extract_method(
         <<"/get/users/pippo/email">>, AppCtx))
  end.

compile(_) ->
  fun() ->
    File = [
      {"paths", [
        {"/users/{userid}", [{"get", ok}]},
        {"/users/{userid}/email", [{"get", ok}]},
        {"/my-clients/{clientid}", [{"post", ok}]},
        {"/boxes", [{"get", ok}]},
        {"/boxes/{boxid}", [{"get", ok}]},
        {"/not-exists/pippo", [{"post", ok}]}
      ]}
    ],
    RouteCtx = routectx,
    AppCtx = swagger_routerl_emqtt:compile(File, RouteCtx),
    Event1 = <<"/get/users/pippo/email">>,
    Event2 = <<"/get/users/pippo/email2">>,
    Event3 = <<"/get/boxes/not-defined">>,

    meck:new(emqtt_users_userid_email,
             [no_link, passthrough, no_history, non_strict]),
    meck:expect(emqtt_users_userid_email, get, 4,
                fun(MyEvent, empty, ["pippo"], routectx) ->
                    ?assertEqual(<<"/get/users/pippo/email">>, MyEvent),
                    {return, MyEvent}
                end
               ),
    try
      ?assertEqual({return, Event1},
                   swagger_routerl_emqtt:execute(Event1, empty, AppCtx)),
      ?assertEqual({error, notfound},
                   swagger_routerl_emqtt:execute(Event2, empty, AppCtx)),
      ?assertEqual({error, notdefined},
                   swagger_routerl_emqtt:execute(Event3, empty, AppCtx))
    after
      meck:validate(emqtt_users_userid_email),
      meck:unload(emqtt_users_userid_email)
    end,

    Event4 = <<"/post/my-clients/pippo">>,
    meck:new('emqtt_my_clients_clientid',
             [no_link, passthrough, no_history, non_strict]),
    meck:expect('emqtt_my_clients_clientid', post, 4,
                fun(MyEvent, empty, ["pippo"], routectx) ->
                    ?assertEqual(<<"/post/my-clients/pippo">>, MyEvent),
                    {return, MyEvent}
                end
               ),
    try
      ?assertEqual({return, Event4},
                   swagger_routerl_emqtt:execute(Event4, empty, AppCtx))
    after
      meck:validate('emqtt_my_clients_clientid'),
      meck:unload('emqtt_my_clients_clientid')
    end
  end.

