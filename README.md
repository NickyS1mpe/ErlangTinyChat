## ErlangTinyChat
A Ting Chatroom Written in Erlang

## Conclusion
This assignment emphasizes message passing between Erlang processes to create a functional chat application. It covers scenarios related to user interactions, server-client communication, and chatroom management.

## Overview of TinyChat Assignment

### Assignment Goal: 

The goal of the TinyChat assignment is to implement a simple chat application in Erlang. The application involves four primary actors: GUIs, Clients, Server, and Chatrooms.

## User Commands
Users interact with TinyChat using commands entered into the GUI. The supported commands include /join, /leave, /whoami, /nick, and sending messages.

```
/join #chatroom name  /*join chatroom with name “#chatroom name”.*/
/leave  /*leave chatroom who’s tab is currently active/open.*/
/leave #chatroom name  /*leave chatroom with name “#chatroom name”.*/
/whoami  /*what is current nickname?*/
/nick new nickname  /*change nickname from current nickname to “new nickname”. Must start with lowercase*/
some string of text  /*send a message “some string of text” to chatroom who’s tab is currently active/open.*/
/quit  /*quit. Just that user’s GUI and client exit, while server and chatrooms stay running.*/
```

## Processes and Local State
Each actor (Client, Server, Chatroom) has a local state modeled as a parameter in the main loop function. The local state is maintained using record types.

## Message Passing
The assignment defines a protocol for message passing between processes based on user commands. It includes scenarios such as initializing a new client, joining/leaving chatrooms, changing nicknames, sending messages, and quitting.

## Implementation Specifics
- The assignment provides a makefile for building the application.
- The program starts with `main:start()` and can be configured with the number of GUIs.
- The GUI code should not be altered.
- Client nicknames are stored as strings.
- Proper order and handling of messages are crucial for passing test cases.

## Usage Example

This is an outline of the usage of the program (with some assumptions made about the initial client nickname), showing the states of all processes at the end. You can use this as a way to compare that your program does what is expected.

1. `main:start()` is called, spawning 2 GUIs (client 1 with the nickname "client1" and client 2 with the nickname "client2") and 1 server. The GUI for client 1 has the name "gui1", and the GUI for client 2 has the name "gui2".

2. Client 1 issues the command `/nick newclient1`: Client 1 changes the nickname to "newclient1".

3. Client 1 issues the command `/nick newclient1` again: Client 1 tries to change the nickname to "newclient1" again (and fails to do so).

4. Client 1 issues the command `/nick client2`: Client 1 tries to change the nickname to "client2" (and fails to do so).

5. Client 1 issues the command `/join #chat1`: Client 1 joins chatroom 1 "#chat1".

6. Client 1 issues the command `hello` from the #chat1 tab: Client 1 sends the message "hello" to chatroom 1.

7. Client 1 issues the command `/nick newclient1_2`: Client 1 changes the nickname to "newclient1_2".

8. Client 1 issues the command `/join #chat2`: Client 1 joins chatroom 2 "#chat2".

9. Client 1 issues the command `/nick newclient1_3`: Client 1 changes the nickname to "newclient1_3".

10. Client 1 issues the command `/leave #chat1`: Client 1 leaves chatroom #chat1.

11. Client 1 issues the command `/leave #notachatroom`: Client 1 tries to leave some chatroom that does not exist or to which they are not connected (and fails to do so).

12. Client 2 issues the command `/join #chat1`: Client 2 joins chatroom 1.

13. Client 2 issues the command `/join #chat2`: Client 2 joins chatroom 2.

14. Client 2 issues the command `world` from the #chat2 tab: Client 2 sends the message "world" to chatroom 2.

15. Client 1 issues the command `/join #chat1`: Client 1 rejoins chatroom 1.

16. Client 1 issues the command `/join #chat1`:
    Client 1 tries to join chatroom 1 again (and fails to do so).

### The final states of all 5 processes will be as follows:

####
```erlang
• Client 1:
    {cl_st, "gui1","newclient1_3",
      #{"#chat1" => <chat 1 pid>,
        "#chat2" => <chat 2 pid>}}
• Client 2:
    {cl_st, "gui2", "client2",
      #{"#chat1" => <chat 1 pid>,
        "#chat2" => <chat 2 pid>}}
• Chatroom 1:
    {chat_st,"#chat1",
       #{<client 1 pid> => "newclient1_3",
         <client 2 pid> => "client2"},
       [{"newclient1","hello"}]}
• Chatroom 2:
    {chat_st,"#chat2",
       #{<client 1 pid> => "newclient1_3",
         <client 2 pid> => "client2"},
        [{"client2","world"}]}

• Server:
  {serv_st,
     #{<client 1 pid> => "newclient1_3",
       <client 2 pid> => "client2"},
     #{"#chat1" => [<client 1 pid>,<client 2 pid>],
       "#chat2" => [<client 2 pid>,<client 1 pid>]},
     #{"#chat1" => <chat 1 pid>,
       "#chat2" => <chat 2 pid>}}
```
