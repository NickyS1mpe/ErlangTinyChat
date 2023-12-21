-module(server).

-export([start_server/0]).

-include_lib("./defs.hrl").

-spec start_server() -> _.
-spec loop(_State) -> _.
-spec do_join(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_leave(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_new_nick(_State, _Ref, _ClientPID, _NewNick) -> _.
-spec do_client_quit(_State, _Ref, _ClientPID) -> _NewState.

start_server() ->
    catch (unregister(server)),
    register(server, self()),
    case whereis(testsuite) of
        undefined -> ok;
        TestSuitePID -> TestSuitePID ! {server_up, self()}
    end,
    loop(
        #serv_st{
            %% nickname map. client_pid => "nickname"
            nicks = maps:new(),
            %% registration map. "chat_name" => [client_pids]
            registrations = maps:new(),
            %% chatroom map. "chat_name" => chat_pid
            chatrooms = maps:new()
        }
    ).

loop(State) ->
    receive
        %% initial connection
        {ClientPID, connect, ClientNick} ->
            NewState =
                #serv_st{
                    nicks = maps:put(ClientPID, ClientNick, State#serv_st.nicks),
                    registrations = State#serv_st.registrations,
                    chatrooms = State#serv_st.chatrooms
                },
            loop(NewState);
        %% client requests to join a chat
        {ClientPID, Ref, join, ChatName} ->
            NewState = do_join(ChatName, ClientPID, Ref, State),
            loop(NewState);
        %% client requests to join a chat
        {ClientPID, Ref, leave, ChatName} ->
            NewState = do_leave(ChatName, ClientPID, Ref, State),
            loop(NewState);
        %% client requests to register a new nickname
        {ClientPID, Ref, nick, NewNick} ->
            NewState = do_new_nick(State, Ref, ClientPID, NewNick),
            loop(NewState);
        %% client requests to quit
        {ClientPID, Ref, quit} ->
            NewState = do_client_quit(State, Ref, ClientPID),
            loop(NewState);
        {TEST_PID, get_state} ->
            TEST_PID ! {get_state, State},
            loop(State)
    end.

%% executes join protocol from server perspective
do_join(ChatName, ClientPID, Ref, State) ->
    % check if the chatroom with the ChatName existed or not
    case maps:is_key(ChatName, State#serv_st.chatrooms) of
        % existed
        true ->
            ClientNick = maps:get(ClientPID, State#serv_st.nicks),
            ChatroomPID = maps:get(ChatName, State#serv_st.chatrooms),
            ChatroomPID ! {self(), Ref, register, ClientPID, ClientNick},
            % add the new client to chatroom's registrations list
            NewState = State#serv_st{
                registrations = maps:put(
                    ChatName,
                    [ClientPID | maps:get(ChatName, State#serv_st.registrations)],
                    State#serv_st.registrations
                )
            },
            NewState;
        % not existed
        false ->
            % spawn a new chatroom with the ChatName
            ChatroomPID = spawn(fun() -> chatroom:start_chatroom(ChatName) end),
            NewState = State#serv_st{
                chatrooms = maps:put(ChatName, ChatroomPID, State#serv_st.chatrooms)
            },
            ClientNick = maps:get(ClientPID, NewState#serv_st.nicks),
            ChatroomPID ! {self(), Ref, register, ClientPID, ClientNick},
            NewState1 = NewState#serv_st{
                registrations = maps:put(
                    ChatName,
                    [ClientPID],
                    NewState#serv_st.registrations
                )
            },
            NewState1
    end.

%% executes leave protocol from server perspective
do_leave(ChatName, ClientPID, Ref, State) ->
    ChatPID = maps:get(ChatName, State#serv_st.chatrooms),
    ChatPIDs = maps:get(ChatName, State#serv_st.registrations),
    % remove the client PID from the chatroom's registrations list
    NewChatPID = [X || X <- ChatPIDs, X /= ClientPID],
    NewState = State#serv_st{
        registrations = maps:put(
            ChatName, NewChatPID, State#serv_st.registrations
        )
    },
    ChatPID ! {self(), Ref, unregister, ClientPID},
    ClientPID ! {self(), Ref, ack_leave},
    NewState.

%% executes new nickname protocol from server perspective
do_new_nick(State, Ref, ClientPID, NewNick) ->
    % check if the nickname is already existed
    Result = maps:fold(
        fun(_, Value, Acc) ->
            case Value of
                NewNick -> true;
                _ -> Acc
            end
        end,
        false,
        State#serv_st.nicks
    ),
    case Result of
        % not existed, free to use
        false ->
            NewState = State#serv_st{
                nicks = maps:put(ClientPID, NewNick, State#serv_st.nicks)
            },
            % find all the chatrooms that the client has joined
            % send them all message to update the nickname of client
            maps:fold(
                fun(Key, Value, Acc) ->
                    case lists:member(ClientPID, Value) of
                        true ->
                            maps:get(Key, State#serv_st.chatrooms) !
                                {self(), Ref, update_nick, ClientPID, NewNick};
                        false ->
                            Acc
                    end
                end,
                [],
                State#serv_st.registrations
            ),
            ClientPID ! {self(), Ref, ok_nick},
            NewState;
        % existed, return error
        true ->
            ClientPID ! {self(), Ref, err_nick_used},
            State
    end.

%% executes client quit protocol from server perspective
do_client_quit(State, Ref, ClientPID) ->
    % find all the chatrooms that the client has joined
    % remove the client PID from the all these chatrooms' registrations list
    UpdatedState = maps:fold(
        fun(Key, Value, AccState) ->
            case lists:member(ClientPID, Value) of
                true ->
                    ChatPID = maps:get(Key, State#serv_st.chatrooms),
                    ChatPID ! {self(), Ref, unregister, ClientPID},
                    ChatPIDs = maps:get(Key, State#serv_st.registrations),
                    NewChatPID = [X || X <- ChatPIDs, X /= ClientPID],
                    AccState#serv_st{
                        registrations = maps:put(Key, NewChatPID, AccState#serv_st.registrations)
                    };
                false ->
                    AccState
            end
        end,
        State,
        State#serv_st.registrations
    ),
    NewState = UpdatedState#serv_st{
        nicks = maps:remove(ClientPID, UpdatedState#serv_st.nicks),
        registrations = UpdatedState#serv_st.registrations
    },
    ClientPID ! {self(), Ref, ack_quit},
    NewState.
