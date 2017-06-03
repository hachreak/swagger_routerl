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
%%% @doc Transform the swagger file into a routing table.
%%% @end
-module(swagger_routerl_router).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  compile/3,
  dispatch/3,
  get_routectx/1,
  routes/2
]).

-export_type([routectx/0, appctx/0, routes/0, event/0]).

-type yaml()          :: swagger_routerl:yaml().
-type routes()        :: list({re:mp(), handler()}).
-type handler()       :: atom().
-type routectx()      :: term().
-type appctx()        :: #{routectx => routectx(), routes => routes()}.
-type req()           :: cowboy_req:req().
-type event()         :: map().
-type params()        :: swagger_routerl_utils:params().
-type path()          :: swagger_routerl_utils:path().
-type swagger_paths() :: list({list(), any()}).

-ifdef(TEST).
-compile(export_all).
-endif.

%%% API functions

-spec compile(list(), yaml(), routectx()) -> appctx().
compile(Prefix, Yaml, RouteCtx) ->
  Paths = proplists:get_value("paths", Yaml, []),
  Routes = routes(Paths, Prefix),
  build_context(Routes, RouteCtx).

-spec routes(swagger_paths(), list()) -> routes().
routes(Paths, Prefix) ->
  lists:map(fun({SwaggerPath, _Config}) ->
      {build_regex(SwaggerPath), get_filename(Prefix, SwaggerPath)}
    end, Paths).

-spec dispatch(event(), any(), appctx()) ->
    {reply, term(), req(), routectx()}
  | {ok, req(), routectx()}
  | {error, atom(), req(), routectx()}.
dispatch(Event, Req, AppContext) ->
  Routes = maps:get(routes, AppContext),
  RouteCtx = maps:get(routectx, AppContext),
  case match(maps:get(<<"path">>, Event), Routes) of
    {error, _}=Error -> Error;
    {ok, {Handler, Params}} ->
      Method = swagger_routerl_utils:to_atom(maps:get(<<"method">>, Event)),
      try
        Handler:Method(Event, Req, Params, RouteCtx)
      catch
        error:undef -> {error, endpoint_undefined, Req, RouteCtx}
      end
  end.

-spec get_routectx(appctx()) -> routectx().
get_routectx(#{routectx := RouteCtx}) -> RouteCtx.

%%% Private functions

-spec build_context(routes(), routectx()) -> appctx().
build_context(Routes, RouteCtx) ->
  #{
    % routing table compiles
    routes => Routes,
    % this context will be passed to `swagger_routerl_cowboy_ws`
    routectx => RouteCtx
  }.

-spec match(path(), routes()) ->
  {ok, {handler(), params()}} | {error, notfound}.
match(_Path, []) ->
  {error, notfound};
match(Path, [{MP, Handler} | Rest]) ->
  case re:run(Path, MP) of
    {match, Matches} ->
      {ok, {Handler, swagger_routerl_utils:extract_params(Path, Matches)}};
    _Rest -> match(Path, Rest)
  end.

-spec build_regex(list()) -> re:mp().
build_regex(SwaggerPath) ->
  swagger_routerl_utils:swaggerpath_build_regex(SwaggerPath).

-spec get_filename(list(), list()) -> atom().
get_filename(Prefix, PathConfig) ->
  swagger_routerl_utils:swaggerpath2module(Prefix, PathConfig).
