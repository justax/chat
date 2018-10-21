## Erlang Chat

Simple erlang chat using Cowboy websocket protocol. User can use default room or join/create to the new one on connection. Messages come to all users within certain room. Also some sort of bot leave in each room.

## Installation

```bash
git clone https://github.com/justax/chat.git

rebar get-deps
rebar complie

erl -pa ebin -pa deps/*/ebin -s chat

```