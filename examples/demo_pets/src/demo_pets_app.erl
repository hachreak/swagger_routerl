%%%-------------------------------------------------------------------
%% @doc demo_pets public API
%% @end
%%%-------------------------------------------------------------------

-module(demo_pets_app).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-type appctx() :: any().

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  Filename = swagger_filename(),
  Yaml = swagger_routerl:load(Filename),
  {ok, SwaggerFileRaw} = file:read_file(Filename),

  http(Yaml, SwaggerFileRaw),
  tcp(Yaml),

  demo_pets_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

tcp(Yaml) ->
  {Name, Port, Handler, AppCtx} = swagger_routerl_tcp:compile(
    "demo_pets_socket_", Yaml, #{}, #{
      name => "demo_pets_service", port => 12345
    }),

  {ok, _} = ranch:start_listener(
    Name, 100, ranch_tcp, [{port, Port}], Handler, AppCtx).

http(Yaml, SwaggerFileRaw) ->
  Dispatch = cowboy_router:compile([
    {'_', routes(#{protocol => http}, Yaml, SwaggerFileRaw)}
  ]),
  {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
    {env, [{dispatch, Dispatch}]}
  ]).

-spec routes(appctx(), list(), binary()) -> cowboy_router:routes().
routes(#{protocol := Protocol}, Yaml, SwaggerFileRaw) ->
  FileEndpoint = swagger_routerl_cowboy_rest:file_endpoint(
    SwaggerFileRaw, #{endpoint => endpoint(Yaml),
    protocol => swagger_routerl_utils:to_binary(Protocol)}),
  Db = ets:new(pets, [set, named_table, public]),
  RestEndpoints = swagger_routerl_cowboy_rest:compile(
    "demo_pets_rest_", Yaml, #{db => Db}),
  WSEndpoint = swagger_routerl_cowboy_ws:compile(
    "demo_pets_socket_", Yaml, #{}, #{
    handler => swagger_routerl_cowboy_v1_ws_json_dispatcher
  }),

  FileEndpoint ++ RestEndpoints ++ WSEndpoint.

swagger_filename() ->
  PrivDir = code:priv_dir(demo_pets),
  Filename = "swagger.yaml",
  filename:join([PrivDir, Filename]).

endpoint(Yaml) ->
  Version = swagger_routerl:get_version(Yaml),
  "/" ++ Version ++ "/docs/swagger.yaml".
