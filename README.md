swagger_routerl
===============

Routing library that generate the routing table from `swagger.yaml`.

Implemented plugins:

- Cowboy REST routing table.

Cowboy REST plugin
------------------

This is an example how to load the routing table:

```erlang
start(_StartType, _StartArgs) ->
  swagger_routerl:init(),
  File = swagger_routerl:load("swagger.yaml"),
  RestCtx = myctx,
  RoutingTable = swagger_routerl_cowboy_rest(File, RestCtx),
  Dispatch = cowboy_router:compile([{'_', RoutingTable}]),
  {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
                              [{env, [{dispatch, Dispatch}]}]),
  myhttpserver:start_link().
```

Build
-----

    $ rebar3 compile
