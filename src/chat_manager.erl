% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% chat manager
% handle common state and events
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

-module(chat_manager).

-behaviour(gen_server).

-export([start_link/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([terminate_room_clients/1]).

-record(state, {clients, rooms}).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% start manager, register locally
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
	
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% init manager
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
init([]) ->
	% store with the connected clients
	Clients = ets:new(clients, [public]),
	% store with the available rooms
	Rooms = ets:new(rooms, [public]),
	{ok, #state{clients=Clients, rooms=Rooms}}.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% create or choose an existing room
% returns internal name of the room 
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
handle_call({join_room, RoomName, ClientName, ClientPid}, _From, State=#state{rooms=Rooms, clients=Clients}) ->	
	% lookup for the room requested
	{SafeName, RID} = case ets:lookup(Rooms, RoomName) of 
		% have one, return internal name and process id
		[{RoomName, {InternalName, Pid}}|_] -> {InternalName, Pid};
		_ -> 
			% test if we should use default room
			if bit_size(RoomName) =:= 0
				-> {default, whereis(default)};
				true ->
					% gen new internal name for the room since we can't be sure in user choice
		    		NameAtom = list_to_atom(lists:flatten(io_lib:format("room_~p", [rand:uniform(10000)]))),
					% start new room under the supervisor
					{ok, RoomPid} = chatroom_sup:start_room(NameAtom),
					erlang:monitor(process, RoomPid),
					% store new room
					ets:insert(Rooms, {RoomName, {NameAtom, RoomPid}}),
					{NameAtom, RoomPid}
		    end
	end,
	% lookup the connected client
	case ets:lookup(Clients, ClientName) of 
		[] -> 
			% got new one - joining
			ets:insert(Clients, {ClientName, {ClientPid, RoomName, RID}}),
            gen_server:cast(SafeName, {join, ClientName, ClientPid}),
			{reply, {ok, SafeName}, State};
		_ ->
			% already connected, return error  
			{reply, {error, user_connected}, State}
	end;


handle_call(_Event, _From, State) ->
	{reply, ok, State}.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% take the user off the room, clean up store
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
handle_cast({leave_room, RoomName, ClientName, ClientPid}, State=#state{clients=Clients}) ->	
	ets:delete(Clients, ClientName),
	RName = if bit_size(RoomName) =:= 0 -> default; true -> RoomName end,
	gen_server:cast(RName, {leave, ClientName, ClientPid}),
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% got one of own chat room down, notify clients
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
handle_info({'DOWN', _, process, Pid, _Reason}, State=#state{rooms=Rooms, clients=Clients}) ->
	% notify room clients about such sad event
	true = ets:match_delete(Rooms, {'_', {'_', Pid}}),
	L = ets:match(Clients, {'_', {'$1', '_', Pid}}),
	CList = [P || [P] <- L],
	spawn(?MODULE, terminate_room_clients, [CList]),
	{noreply, State};
	
handle_info(_Event, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_, State, _Extra) ->
	{ok, State}.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% close client connections to the room
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
terminate_room_clients([]) ->
	ok;
terminate_room_clients([C|L]) ->
	C ! close,
	terminate_room_clients(L).
