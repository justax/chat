% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% instance for the concrete chat room
% it manages internal bot and resends messages to the room's users
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

-module(chat_room).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-export([send_to_users/2]).

-record(state, {name, bot, users = [], messages = []}).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% start room, register locally
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
start_link(Name) ->
	gen_server:start_link({local, Name}, ?MODULE, [Name], []).
	
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% init room
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
init([Name]) ->
	BotName = list_to_atom(atom_to_list(Name) ++ "_bot"),
	erlang:process_flag(trap_exit, true),
	{ok, Bot} = bot:start_link(Name, BotName),
	{ok, #state{name=Name, bot=Bot}}.

handle_call(_Event, _From, State) ->
	{reply, ok, State}.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% join the user to the room
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
handle_cast({join, Name, Pid}, State=#state{users=Users, bot=Bot}) ->
	NewState=State#state{users=[{Name, Pid}|Users]},
	gen_fsm:send_event(Bot, {joined, Name}),
	{noreply, NewState};

handle_cast({leave, Name, Pid}, State=#state{users=Users}) ->
	NewState=State#state{users=lists:delete({Name, Pid}, Users)},
	{noreply, NewState};

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% got message from the websocket connection for this room
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -	
handle_cast({msg, Name, Msg}, State=#state{users=Users}) ->
	% notify all the room users with the new message
	send_to_users(Users, {Name, Msg}),
	{noreply, State};

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% got message from the bot, send to all room users
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
handle_cast({bot_msg, Name, Msg}, State=#state{users=Users}) ->
	% notify all the room users with the new message
	send_to_users(Users, {Name, Msg}),
	{noreply, State}.

handle_info(_Event, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_, State, _Extra) ->
	{ok, State}.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% notify all the room users with the new message
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
send_to_users([{_, Pid}|Users], {Name, Msg}) ->	
	Pid ! {msg, {Name, Msg}},
	send_to_users(Users, {Name, Msg});
send_to_users([], _) ->
	ok.








