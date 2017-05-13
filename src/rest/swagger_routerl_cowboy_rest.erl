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
%%% @doc Transform the swagger file into a Cowboy REST routing table.
%%% @end
-module(swagger_routerl_cowboy_rest).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  compile/3,
  file_endpoint/2
]).

-export_type([routectx/0]).

-type routectx() :: term().
-type routes()   :: cowboy_router:routes().
-type swagger_endpoint_ctx() :: #{protocol := _, endpoint := _, handler := _}.
-type yaml()     :: swagger_routerl:yaml().

-ifdef(TEST).
-compile(export_all).
-endif.

%%% API functions

-spec compile(string(), yaml(), routectx()) -> routes().
compile(Prefix, Yaml, RouteContext) ->
  BasePath = proplists:get_value("basePath", Yaml, ""),
  Paths = proplists:get_value("paths", Yaml),
  lists:map(
    fun({SwaggerPath, _Config}) ->
        {get_route(BasePath, SwaggerPath),
         get_filename(Prefix, SwaggerPath),
         RouteContext}
    end, Paths).

-spec file_endpoint(binary(), swagger_endpoint_ctx()) -> routes().
file_endpoint(SwaggerFileRaw, Ctx) ->
  Protocol = maps:get(protocol, Ctx, <<"http">>),
  Endpoint = maps:get(endpoint, Ctx, "/docs/swagger.yaml"),
  Handler = maps:get(handler, Ctx, swagger_routerl_cowboy_rest_docs_handler),
  SwaggerFile = set_scheme(SwaggerFileRaw, Protocol),
  [{Endpoint, Handler, #{swagger_file => SwaggerFile}}].

%%% Private functions

-spec get_filename(string(), string()) -> atom().
get_filename(Prefix, PathConfig) ->
  swagger_routerl_utils:swaggerpath2module(Prefix, PathConfig).

-spec get_route(string(), string()) -> string().
get_route(BasePath, SwaggerPath) ->
  Opts = [{return,list}, global],
  Path1 = re:replace(SwaggerPath, "{", "[:", Opts),
  CowboyPath = re:replace(Path1, "}", "]", Opts),
  BasePath ++ CowboyPath.

-spec set_scheme(binary(), binary()) -> binary().
set_scheme(SwaggerFileRaw, Protocol) ->
  swagger_routerl_utils:render(
    SwaggerFileRaw, [{<<"scheme_protocol">>, Protocol}]).
