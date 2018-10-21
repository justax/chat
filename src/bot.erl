-module(bot).
-bahavior(gen_fsm).
-export([
	stop/1,
	start_link/2, 
	handle_event/3,
	handle_sync_event/4,
	handle_info/3,
	terminate/3,
	code_change/4]).

-export([
	init/1, 
	idle/2,
	welcome/2
]).

% defined bot responses
-define(MSG, [
	"i'm so boring...", 
	"what's a man?!", 
	"one day i will show you something interesting...",
	"hey! where is the door?!", 
	"does any body know what the 'machine rising' means?",
	"it's awesome!",
	"let's talk about..."]).
% define bot state
-record(state, {room, name}).

start_link(Room, Name) ->
	gen_fsm:start_link({local, Name}, ?MODULE, [Room, Name], []).

stop(Name) ->
	gen_fsm:send_all_state_event(Name, stop).

init([Room, Name]) ->
	{ok, idle, #state{room=Room, name=Name}, 2000}.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% new client was joined, welcome it
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
idle({joined, Name}, State) ->
	do_welcome(Name, State),
	{next_state, welcome, State, 2000};

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% send random message by timeout
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
idle(timeout, State) ->
	do_random_msg(State),
	{next_state, idle, State, 5000 + rand:uniform(30000)};

idle(_, State) ->
		{next_state, idle, State, 0}.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% new client was joined, welcome it
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
welcome({joined, Name}, State) ->
	do_welcome(Name, State),
	{next_state, welcome, State, 10000};

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% move to idle state by timeout
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
welcome(timeout, State) ->
	{next_state, idle, State, 5000 + rand:uniform(30000)};

welcome(_, State) ->
	{next_state, welcome, State, 10000}.

handle_event(stop, _StateName, State) ->
	{stop, normal, State};
handle_event(_Event, StateName, State) ->
	{next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.
 
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.
 
terminate(_Reason, _StateName, _State) ->
    ok.
 
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

do_random_msg(State) ->
	L = length(?MSG),
	Idx = rand:uniform(L),
	do_msg(lists:nth(Idx, ?MSG), State).

do_welcome(Name, State) ->
	Msg = io_lib:format("hey ~s! how's it going there, outside?", [Name]),
	do_msg(Msg, State).

do_msg(Msg, #state{room=Room, name=Name}) ->
	gen_server:cast(Room, {bot_msg, Name, Msg}).


