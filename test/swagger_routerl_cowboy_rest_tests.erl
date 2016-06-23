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
-module(swagger_routerl_cowboy_rest_tests).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-include_lib("eunit/include/eunit.hrl").

swagger_routerl_cowboy_rest_test_() ->
  {foreach,
    fun start/0,
    fun stop/1,
    [
     fun get_route/1,
     fun get_filename/1,
     fun compile/1
    ]
  }.

start() -> ok.

stop(_) -> ok.

get_route(_) ->
  fun() ->
    Path = swagger_routerl_cowboy_rest:get_route(
        "api/0.1.0", "/users/{userid}/email"),
    ?assertEqual(
      "api/0.1.0/users/[:userid]/email", Path
    )
  end.

get_filename(_) ->
  fun() ->
    Path = swagger_routerl_cowboy_rest:get_filename(
        "resource_", "/users/{userid}/email"),
    ?assertEqual(
      resource_users_userid_email, Path
    )
  end.

compile(_) ->
  fun() ->
    File = [
      {"basePath", "/api/0.1.0"},
      {"paths", [
        {"/users/{userid}", ok},
        {"/users/{userid}/email", ok},
        {"/clients/{clientid}", ok},
        {"/boxes", ok},
        {"/boxes/{boxid}", ok}
      ]}
    ],
    Routes = swagger_routerl_cowboy_rest:compile("resource_", File, ctx),
    ?assertEqual([
      {"/api/0.1.0/users/[:userid]", resource_users_userid, ctx},
      {"/api/0.1.0/users/[:userid]/email", resource_users_userid_email, ctx},
      {"/api/0.1.0/clients/[:clientid]", resource_clients_clientid, ctx},
      {"/api/0.1.0/boxes", resource_boxes, ctx},
      {"/api/0.1.0/boxes/[:boxid]", resource_boxes_boxid, ctx}
    ], Routes)
  end.
