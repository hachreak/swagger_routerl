swagger_routerl
===============

Routing library that generate the routing table from `swagger.yaml`.

Implemented plugins:

- Cowboy REST routing table.
- Cowboy Websocket routing table: dispatch events on top of a
  websocket endpoint.
- Raw tcp: dispatch events on top of a socket.

How configure plugins
---------------------

See example `demo_pets`.

Build
-----

    $ rebar3 compile

Test
----

    $ ./run-tests.sh
