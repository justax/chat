% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% chat application
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

-module(chat).
-behaviour(application).
-behaviour(supervisor).

-export([start/0, stop/0]).
-export([start/2, stop/1]).
-export([init/1]).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% application easy start
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
start() ->
	application:ensure_all_started(?MODULE),
	% start cowboy tcp server
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, chat, "index.html"}},
			{"/ws", chat_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, 
		[{port, 8080}], 
		#{env => #{dispatch => Dispatch}}
		),
	ok.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% stop application
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
stop() ->
    application:stop(?MODULE).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% run main supervisor
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
start(_Type, _Args) ->
    supervisor:start_link({local, chat_sup}, ?MODULE, []).

stop(_State) ->
    ok.
  
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% supervisor initialization
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
init([]) ->
	% start chat manager in supervisor tree
    ChatManager = {chat_manager, 
        {chat_manager, start_link, []}, 
        permanent, infinity, supervisor, []},
	% start rooms manager in supervisor tree
    ChatRoomSup = {chatroom_sup, 
        {chatroom_sup, start_link, []}, 
        permanent, infinity, supervisor, [chatroom_sup]},

	{ok, { {one_for_one, 5, 10}, [
		ChatRoomSup,
		ChatManager
	]} }.
