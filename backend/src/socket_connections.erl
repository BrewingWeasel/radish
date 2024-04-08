-module(socket_connections).
-export([generate_server/0]).

generate_server() -> 
   {ok, Port} = gen_udp:open(0, [{active, false}, {ifaddr, {local,"/tmp/radish_main"}}]),
   handle_connections(Port).

handle_connections(Port) ->
   {ok, {_, _, Data}} = gen_udp:recv(Port, 0),
   erlang:spawn(fun() -> run_shell(Data) end),
   handle_connections(Port).

run_shell(Uuid) ->
   {ok, RequestPort} = gen_udp:open(0, [{active, false}, {ifaddr, {local,"/tmp/radish" ++ Uuid ++ "_request"}}]),
   {ok, Shell} = radish:start_shell(RequestPort),
   {ok, Socket} = gen_udp:open(0, [local]),
   handle_port_for_shell(RequestPort, Uuid, Socket, Shell).

handle_port_for_shell(RequestPort, Uuid, Socket, Shell) ->
   {ok, {_, _, Data}} = gen_udp:recv(RequestPort, 0),
   Response = radish_shell:run_command(Shell, Data),
   gen_udp:send(Socket, {local,"/tmp/radish" ++ Uuid ++ "_response"}, 0, Response),
   handle_port_for_shell(RequestPort, Uuid, Socket, Shell).
