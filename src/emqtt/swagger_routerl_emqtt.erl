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
%%% @doc Transform the swagger file into a MQTT Application Layer.
%%%
%%% Through Swagger you can define endpoints connected to the topics.
%%%
%%% e.g. the topic /get/users/fuu will be equivalent to the REST GET /users/fuu
%%% @end
-module(swagger_routerl_emqtt).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([compile/2, get_routectx/1, execute/3]).

-export_type([routectx/0]).

-type yaml()     :: swagger_routerl:yaml().
-type routes()   :: list({re:mp(), atom()}).
-type routectx() :: term().
-type appctx()   :: #{routectx => routectx(), routes => routes()}.
-type payload()  :: binary().
-type params()   :: swagger_routerl_utils:params().
-type topic()    :: swagger_routerl_utils:url().
-type handler()  :: atom().
-type method()   :: binary().

-ifdef(TEST).
-compile(export_all).
-endif.

%%% API functions

-spec compile(yaml(), routectx()) -> appctx().
compile(Yaml, RouteCtx) ->
  Paths = proplists:get_value("paths", Yaml),
  % merge the list of list in a single list
  Routes = lists:merge(
    % Return [ [{MP, module}, ..], ..]
    lists:map(
    fun({SwaggerPath, Config}) ->
        Module = get_filename(SwaggerPath),
        % return [{MP, module}, ..]
        lists:map(fun(MP) ->
            {MP, Module}
        end, build_regex(SwaggerPath, Config))
    end, Paths)),
  build_context(Routes, RouteCtx).

% TODO extend to bi-directionality stream.
-spec execute(topic(), payload(), appctx()) -> ok | error.
execute(Topic, Payload, #{routes := Routes, routectx := RouteCtx}=AppCtx) ->
  case match(Topic, Routes) of
    {error, _}=Error -> Error;
    {ok, {Handler, Params}} ->
      Method = swagger_routerl_utils:to_atom(extract_method(Topic, AppCtx)),
      try
        Handler:Method(Topic, Payload, Params, RouteCtx)
      catch
        error:undef -> {error, notdefined}
      end
  end.

%% Private functions

-spec extract_method(topic(), appctx()) -> method().
extract_method(Topic, #{extract_method := MP}) ->
  case re:run(Topic, MP) of
    nomatch -> throw(wrong_message);
    {match, [_, {Init, End}, _Rest]} -> binary:part(Topic, Init, End)
  end.

-spec match(topic(), routes()) ->
  {ok, {handler(), params()}} | {error, notfound}.
match(_Topic, []) ->
  {error, notfound};
match(Topic, [{MP, Handler} | Rest]) ->
  case re:run(Topic, MP) of
    {match, Matches} ->
      {ok, {Handler, swagger_routerl_utils:extract_params(Topic, Matches)}};
    _Rest -> match(Topic, Rest)
  end.

% @doc Build a list of regex
-spec build_regex(string(), list({string(), list()})) -> list(re:mp()).
build_regex(SwaggerPath, Config) ->
  lists:map(
    fun(Prefix) ->
        swagger_routerl_utils:swaggerpath_build_regex(SwaggerPath, Prefix)
    end, proplists:get_keys(Config)).

-spec get_filename(list()) -> atom().
get_filename(PathConfig) ->
  swagger_routerl_utils:swaggerpath2module("emqtt_", PathConfig).

-spec build_context(routes(), routectx()) -> appctx().
build_context(Routes, RouteCtx) ->
  {ok, MP} = re:compile("^/([\\w|-]+)/(.*)$"),
  #{
    % routing table compiles
    routes => Routes,
    % this context will be passed to the route
    routectx => RouteCtx,
    % compiled regex to extract the method from the topic
    extract_method => MP
  }.

-spec get_routectx(appctx()) -> routectx().
get_routectx(#{routectx := RouteCtx}) -> RouteCtx.
