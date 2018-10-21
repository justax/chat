% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% cowboy websocket handler
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

-module(chat_handler).

-export([
    init/2,
    websocket_init/1, websocket_handle/2,
    websocket_info/2,
    terminate/3
]).

% define state with room and user names
-record(state, {room, name}).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% init handler
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
init(Req, _State) ->
    % generate random if the user name is not defined
    Id = integer_to_binary(rand:uniform(100000)),
    % takes room and user names from the request
    #{room := Room, name := Name} = cowboy_req:match_qs([
        {room, [], <<"default">>}, 
        {name, [], <<"user", Id/binary>>}], Req),
    {cowboy_websocket, Req, #state{room=Room, name=Name}, 
        #{
            idle_timeout => infinity,
            max_frame_size => 1024
        }}.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% websocket init
% get connection to the room from manager
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
websocket_init(State=#state{room=Room, name=Name}) ->
    case gen_server:call(chat_manager, {join_room, Room, Name, self()}) of
        {ok, SafeRoomName} -> 
            {ok, #state{room={SafeRoomName, Room}, name=Name}};
        {error, user_connected} ->
            % drop connected if user already connected
            {reply, {close, 1000, <<"user already connected">>}, State}
    end.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% handle message from the user
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
websocket_handle({text, Msg}, State=#state{room={Room,_}, name=Name}) ->
    % resend message to the room service to handle common events
    gen_server:cast(Room, {msg, Name, Msg}),
    {ok, State, hibernate};

websocket_handle(_Any, State) ->
    {ok, State, hibernate}.

websocket_info({timeout, _Ref, Msg}, State) ->
    {reply, {text, Msg}, State, hibernate};

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% echo response with the nick name to display that on the client
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
websocket_info({msg, {NickName, Msg}}, State) ->
    Response = io_lib:format("[~s] ~s", [NickName, Msg]),
    {reply, {text, Response}, State, hibernate};

websocket_info(close, State) ->
    {stop, State};
websocket_info(_Info, State) ->
    {ok, State, hibernate}.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% got socket termination, making clean up via manager
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
terminate(_, _, _State=#state{room= <<>>, name=Name}) ->
    gen_server:cast(chat_manager, {leave_room, <<>>, Name, self()});
terminate(_, _, _State=#state{room={Room,_}, name=Name}) ->
    gen_server:cast(chat_manager, {leave_room, Room, Name, self()}),
    ok.
