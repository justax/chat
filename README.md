## Erlang Chat

Simple erlang chat using Cowboy websocket protocol. User can use default room or join/create to the new one on connection. Messages come to all users within the certain room. Also some sort of the bot leaves in each room.

## Usage

```
git clone https://github.com/justax/chat.git
cd chat
rebar get-deps
rebar compile
erl -pa ebin -pa deps/*/ebin -s chat
```

