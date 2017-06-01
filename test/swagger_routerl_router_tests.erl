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
%%% @doc Test router
%%% @end
-module(swagger_routerl_router_tests).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-include_lib("eunit/include/eunit.hrl").


context_test() ->
    AppCtx = swagger_routerl_router:build_context(qwerty, bar),
    ?assertEqual(
      bar, swagger_routerl_router:get_routectx(AppCtx)).

build_regex_test() ->
    MP = swagger_routerl_router:build_regex(
        "/users/{userid}/email"),
    Handler = fuu,
    ?assertEqual({ok, {Handler, ["abc"]}}, swagger_routerl_router:match(
      <<"/users/abc/email">>, [{MP, Handler}])),
    ?assertEqual({ok, {Handler, ["abc-def"]}}, swagger_routerl_router:match(
      <<"/users/abc-def/email">>, [{MP, Handler}])),
    ?assertEqual(
      {ok, {Handler, ["abc-123-def"]}},
      swagger_routerl_router:match(
        <<"/users/abc-123-def/email">>, [{MP, Handler}])),
    ?assertEqual({error, notfound}, swagger_routerl_router:match(
      <<"/users">>, [{MP, Handler}])),
    ?assertEqual({error, notfound}, swagger_routerl_router:match(
      <<"/users/">>, [{MP, Handler}])),
    ?assertEqual({error, notfound}, swagger_routerl_router:match(
      <<"/users/abc/fuu">>, [{MP, Handler}])),
    ?assertEqual({error, notfound}, swagger_routerl_router:match(
      <<"/users/abc/email/fuu">>, [{MP, Handler}])),

    % check "-"
    MP2 = swagger_routerl_router:build_regex(
        "/my-users/{userid}/email"),
    Handler2 = bar,
    ?assertEqual({ok, {Handler2, ["abc"]}}, swagger_routerl_router:match(
      <<"/my-users/abc/email">>, [{MP2, Handler2}])).

get_filename_test() ->
    Path = swagger_routerl_router:get_filename(
        "ws_", "/users/{userid}/email"),
    ?assertEqual(
      ws_users_userid_email, Path
    ).

match_test() ->
    Url = <<"/my-clients/pippo">>,
    {ok, MP} = re:compile("my-clients/([\\w|-]+)"),
    {ok, {myhandler, Params}} = swagger_routerl_router:match(
                                  Url, [{MP, myhandler}]),
    ?assertEqual(["pippo"], Params).
