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
  File = swagger_routerl:load("swagger.yaml"),
  RestCtx = myctx,
  RoutingTable = swagger_routerl_cowboy_rest:routes(File, RestCtx),
  Dispatch = cowboy_router:compile([{'_', RoutingTable}]),
  {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
                              [{env, [{dispatch, Dispatch}]}]),
  myhttpserver:start_link().
```

Cowboy Websocket plugin
-----------------------

e.g. We have this routing table defined in our `swagger.yaml` file:

 - `/users/{userid}` which return the information about the user identified by
   the `userid`.

From a Websocket client, you'll send a `JSON` message to our
`websocket server`, called `ws_handler`, which contains:

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
  File = swagger_routerl:load("swagger.yaml"),
  % Context passed to the Websocket handler (in this example,
  % `ws_users_userid.erl`)
  RouteCtx = myctx,
  % compile the routing table
  Routes = swagger_routerl_cowboy_compile:routes(File),
  % build the application context for `swagger_routerl_cowboy_ws`
  AppCtx = #{
      % routing table compiles
      routes => Routes,
      % this context will be passed to `swagger_routerl_cowboy_ws`
      routectx => RouteCtx
  },
  % compile `cowboy` routing table
  Dispatch = cowboy_router:compile([
    {'_', [
      % the websocket dispatcher
      {"/websocket", ws_handler, [AppCtx]}
    ]}
  ])
  % start cowboy
  {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
                              [{env, [{dispatch, Dispatch}]}]),
  myhttpserver:start_link().
```

The `ws_handler.erl` file which dispatch the incoming requests:

```erlang
init(Req, AppCtx) ->
  {cowboy_websocket, Req, AppCtx}.

websocket_handle({text, EventTxt}, Req, AppCtx) ->
  % decode event from a websocket client
  Event = jsx:decode(EventTxt, [return_maps]),
  % dispatch the request
  swagger_routerl_cowboy_ws:execute(Event, Req, Appctx).
```

The handler for the path `/users/{userid}`, `ws_users_userid.erl`:

```erlang
-module(ws_users_userid).

-export([get/2]).

% finally, handle the websocket request
get(Event, Req, AppCtx) ->
  ...
```

Build
-----

    $ rebar3 compile
