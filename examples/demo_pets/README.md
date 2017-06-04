demo_pets
=========

Example application that implement REST, Websocket and TCP API with the help of `swagger_routerl`.

###  Swagger file endpoint

At the address `http://127.0.0.1:8080/docs/swagger.yaml` we can find the file.

### Connect through TCP from python

```python
import socket
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect(('0.0.0.0', 12345))

s.send('{"path": "/pets/pippo", "method": "put", "pet": {"name": "pippo", "description": "My pet!"}}')
print(s.recv(255))
# print: {"context":{},"result":"ok"}

s.send('{"path": "/pets/pippo", "method": "get"}')
print(s.recv(255))
# print: {"context":{"description":"My pet!","name":"pippo"},"result":"ok"}

s.send('{"path": "/pets/pippo", "method": "delete"}')
print(s.recv(255))
# print: {"context":{},"result":"ok"}
```

### Connect through Websocket

Open a connection to: `ws://0.0.0.0:8080/websocket`

Send: `{"path": "/pets/pippo", "method": "put", "pet": {"name": "pippo", "description": "My pet!"}}
Receive: `{"context":{},"result":"ok"}`

Send: `{"path": "/pets/pippo", "method": "get"}`
Receive: `{"context":{"description":"My pet!","name":"pippo"},"result":"ok"}`

Send: `{"path": "/pets/pippo", "method": "delete"}`
Receive: `{"context":{},"result":"ok"}`

### Connect through REST

`http PUT http://0.0.0.0:8080/api/pets/pippo name=pippo description="My pet!"`

```
HTTP/1.1 204 No Content
content-length: 0
content-type: application/json
date: Sun, 04 Jun 2017 14:24:00 GMT
server: Cowboy
```

`http GET http://0.0.0.0:8080/api/pets/pippo`

```
HTTP/1.1 200 OK
content-length: 50
content-type: application/json
date: Sun, 04 Jun 2017 14:24:03 GMT
server: Cowboy

{
    "pippo": {
        "description": "My pet!",
        "name": "pippo"
    }
}
```

`http DELETE http://0.0.0.0:8080/api/pets/pippo`

```
HTTP/1.1 204 No Content
content-length: 0
content-type: application/json
date: Sun, 04 Jun 2017 14:27:13 GMT
server: Cowboy
```

Build
-----

    $ rebar3 compile

Run
---

    $ rebar3 shell --apps demo_pets
