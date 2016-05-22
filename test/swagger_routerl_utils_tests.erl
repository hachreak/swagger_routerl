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
%%% @doc Test utils.
%%% @end
-module(swagger_routerl_utils_tests).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
  {foreach,
    fun start/0,
    fun stop/1,
    [
     fun swaggerpath2module/1,
     fun swaggerpath_build_regex/1
    ]
  }.

start() -> ok.

stop(_) -> ok.

swaggerpath2module(_) ->
  fun() ->
    ?assertEqual(
      ws_users_userid,
      swagger_routerl_utils:swaggerpath2module("ws_", "/users/{userid}")
    ),
    ?assertEqual(
      ws_my_users_userid,
      swagger_routerl_utils:swaggerpath2module("ws_", "/my-users/{userid}")
    )
  end.

swaggerpath_build_regex(_) ->
  fun() ->
      % string
      MP = swagger_routerl_utils:swaggerpath_build_regex("/users/{userid}"),
      {match, _} = re:run("/users/hello", MP),
      MP2 = swagger_routerl_utils:swaggerpath_build_regex(
              "/users/{userid}", "/GET"),
      {match, _} = re:run("/GET/users/hello", MP2),
      MP3 = swagger_routerl_utils:swaggerpath_build_regex(
              "/users/{userid}", "GET"),
      {match, _} = re:run("/GET/users/hello", MP3),

      % binary string
      MP = swagger_routerl_utils:swaggerpath_build_regex("/users/{userid}"),
      {match, _} = re:run(<<"/users/hello">>, MP),
      MP2 = swagger_routerl_utils:swaggerpath_build_regex(
              "/users/{userid}", "/GET"),
      {match, _} = re:run(<<"/GET/users/hello">>, MP2),
      MP3 = swagger_routerl_utils:swaggerpath_build_regex(
              "/users/{userid}", "GET"),
      {match, _} = re:run(<<"/GET/users/hello">>, MP3)
  end.
