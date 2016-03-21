swagger_routerl
===============

Routing library that generate the routing table from `swagger.yaml`.

Implemented plugins:

- Cowboy REST routing table.
- Cowboy Websocket routing table.

Cowboy REST plugin
------------------

This is an example how to load the routing table:

```erlang
start(_StartType, _StartArgs) ->
  swagger_routerl:init(),
  Yaml = swagger_routerl:load("swagger.yaml"),
  RestCtx = myctx,
  RoutingTable = swagger_routerl_cowboy_rest:routes(Yaml, RestCtx),
  Dispatch = cowboy_router:compile([{'_', RoutingTable}]),
  {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
                              [{env, [{dispatch, Dispatch}]}]),
  myhttpserver:start_link().
```

Cowboy Websocket plugin
-----------------------

E.g. We have this routing table defined in our `swagger.yaml` file:

 - `/users/{userid}` which return the information about the user identified by
   the `userid`.

From a websocket, clients can send a `JSON` message to our
`websocket server` and own `ws_users_userid.erl` will handle and process it:

```json
{
  "url": "/users/1",
  "method": "get"
}
```

Now, we'll se how implement the server:

This is an example how to load the routing table, in the application
`websocket_app.erl`:

```erlang
start(_StartType, _StartArgs) ->
  % init the module
  swagger_routerl:init(),
  % load the swagger file
  Yaml = swagger_routerl:load("swagger.yaml"),
  % Context passed to the Websocket handler (in this example the handler is
  % only `ws_users_userid.erl`)
  RouteCtx = myctx,
  % compile the routing table
  Routes = swagger_routerl_cowboy_ws:compile(Yaml),
  % build the application context for `swagger_routerl_cowboy_ws`
  AppCtx = swagger_routerl_cowboy_ws:build_context(Routes, RouteCtx),
  % compile `cowboy` routing table
  Dispatch = cowboy_router:compile([
    {'_', [
      % the websocket dispatcher (you can choose the endpoint)
      {"/websocket", swagger_routerl_cowboy_ws_dispatcher, [AppCtx]}
    ]}
  ])
  % start cowboy
  {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
                              [{env, [{dispatch, Dispatch}]}]),
  myhttpserver:start_link().
```

After cowboy is correctly configured, we can implement the handler for the
url `/users/{userid}`: `ws_users_userid.erl`

```erlang
-module(ws_users_userid).

-export([get/2]).

-type appctx() :: swagger_routerl_cowboy_ws:appctx().

% finally, handle the websocket request
get(Event, Req, RouteCtx) ->
  myctx = RouteCtx,
  ...
  {ok, Req, RouteCtx}.
```

Build
-----

    $ ./utils/rebar3 compile

Test
----

    $ ./utils/rebar3 eunit
