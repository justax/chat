% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% chat rooms supervisor
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
-module(chatroom_sup).
-behaviour(supervisor).

-export([start_link/0, init/1, start_room/1
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	ChildSpec = {default, {chat_room, start_link, [default]},
		permanent, 5000, worker, [chat_room]},
	{ok, { {one_for_one, 5, 10}, [
		ChildSpec
	]} }.

start_room(Name) ->
	ChildSpec = {Name,
		{chat_room, start_link, [Name]},
		temporary, 5000, worker, [chat_room]},
	supervisor:start_child(?MODULE, ChildSpec).