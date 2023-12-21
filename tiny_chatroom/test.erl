-module(test).
-export([start/0, start/1, all_states/1]).
-include_lib("./defs.hrl").

collect_clients(0) ->
    [];
collect_clients(N) ->
    receive
        {client_up, From} -> [From | collect_clients(N - 1)]
    end.

%% Starts the server and N (default 2) clients.
%% Returns a {ServerPID, ClientPID[]} tuple.
start() -> start(2).
start(N) ->
    register(testsuite, self()),
    main:start(N),
    S =
        receive
            {server_up, From} -> From
        end,
    Clients = collect_clients(N),
    {S, Clients}.

collect_client_states(0) ->
    [];
collect_client_states(N) ->
    receive
        {result, _From, _Ref, {get_state, State}} ->
            [State | collect_client_states(N - 1)]
    end.

collect_chatroom_states(0) ->
    [];
collect_chatroom_states(N) ->
    receive
        {get_state, State} -> [State | collect_chatroom_states(N - 1)]
    end.

%% Takes in the return value from `start` and gives back the current
%% state of all relevant processes.
%% {ServerState, ClientState[], ChatRoomState[]} tuple returned.
all_states({ServerPID, ClientPIDs}) ->
    ServerPID ! {self(), get_state},
    ServerState =
        receive
            {get_state, State} -> State
        end,
    [Client ! {request, self(), 0, {get_state}} || Client <- ClientPIDs],
    ChatRooms = maps:values(ServerState#serv_st.chatrooms),
    [ChatRoom ! {self(), get_state} || ChatRoom <- ChatRooms],
    {
        ServerState,
        collect_client_states(length(ClientPIDs)),
        collect_chatroom_states(length(ChatRooms))
    }.
