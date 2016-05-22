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
%%% @doc Transform the swagger file into a Cowboy WebSocket routing table.
%%% @end
-module(swagger_routerl_cowboy_ws).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([compile/2, execute/3, get_routectx/1]).

-export_type([routectx/0, appctx/0, routes/0, params/0]).

-type yaml()     :: swagger_routerl:yaml().
-type routes()   :: list({re:mp(), handler()}).
-type handler()  :: atom().
-type routectx() :: term().
-type appctx()   :: #{routectx => routectx(), routes => routes()}.
-type req()      :: cowboy_req:req().
-type event()    :: map().
-type url()      :: list().
-type params()   :: list(list()).
-type matches()  :: {integer(), integer()}.

-ifdef(TEST).
-compile(export_all).
-endif.

%%% API functions

-spec compile(yaml(), routectx()) -> appctx().
compile(Yaml, RouteCtx) ->
  Paths = proplists:get_value("paths", Yaml),
  Routes = lists:map(
    fun({SwaggerPath, _Config}) ->
        {build_regex(SwaggerPath),
         get_filename(SwaggerPath)}
    end, Paths),
  build_context(Routes, RouteCtx).

-spec execute(event(), req(), appctx()) ->
    {ok, req(), routectx()}
  | {ok, req(), routectx(), hibernate}
  | {reply, cow_ws:frame() | [cow_ws:frame()], req(), routectx()}
  | {reply, cow_ws:frame() | [cow_ws:frame()], req(), routectx(), hibernate}
  | {stop, req(), routectx()}.
execute(Event, Req, AppContext) ->
  Routes = maps:get(routes, AppContext),
  RouteCtx = maps:get(routectx, AppContext),
  case match(maps:get(<<"url">>, Event), Routes) of
    {error, _}=Error -> Error;
    {ok, {Handler, Params}} ->
      Method = to_atom(maps:get(<<"method">>, Event)),
      try
        Handler:Method(Event, Req, Params, RouteCtx)
      catch
        error:undef -> {error, notdefined}
      end
  end.

-spec build_context(routes(), routectx()) -> appctx().
build_context(Routes, RouteCtx) ->
  #{
    % routing table compiles
    routes => Routes,
    % this context will be passed to `swagger_routerl_cowboy_ws`
    routectx => RouteCtx
  }.

-spec get_routectx(appctx()) -> routectx().
get_routectx(#{routectx := RouteCtx}) -> RouteCtx.

%%% Private functions

-spec to_atom(term()) -> atom().
to_atom(Atom) when is_atom(Atom) -> Atom;
to_atom(Binary) when is_binary(Binary) ->
  list_to_atom(binary_to_list(Binary)).


-spec match(url(), routes()) ->
  {ok, {handler(), params()}} | {error, notfound}.
match(_Url, []) ->
  {error, notfound};
match(Url, [{MP, Handler} | Rest]) ->
  case re:run(Url, MP) of
    {match, Matches} -> {ok, {Handler, extract_params(Url, Matches)}};
    _Rest -> match(Url, Rest)
  end.

-spec build_regex(list()) -> re:mp().
build_regex(SwaggerPath) ->
  swagger_routerl_utils:swaggerpath_build_regex(SwaggerPath).

-spec get_filename(list()) -> atom().
get_filename(PathConfig) ->
  swagger_routerl_utils:swaggerpath2module("ws_", PathConfig).

-spec extract_params(url(), matches()) -> params().
extract_params(Url, [_First | Matches]) ->
  [string:substr(
     binary_to_list(Url), Start + 1, Length) || {Start, Length} <- Matches].
