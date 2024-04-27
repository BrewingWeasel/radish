-module(socket_connections).
-export([generate_server/0, finish_command/2, supply_command/3]).

generate_server() -> 
   file:del_dir_r("/tmp/radish"),
   file:make_dir("/tmp/radish"),
   {ok, Port} = gen_udp:open(0, [{active, false}, {ifaddr, {local,"/tmp/radish/main"}}]),
   handle_connections(Port).

handle_connections(Port) ->
   {ok, {_, _, Data}} = gen_udp:recv(Port, 0),
   erlang:spawn(fun() -> run_shell(Data) end),
   handle_connections(Port).

run_shell(Uuid) ->
   {ok, RequestPort} = gen_udp:open(0, [{active, false}, {ifaddr, {local,"/tmp/radish/" ++ Uuid ++ "_request"}}]),
   ResponsePort = "/tmp/radish/" ++ Uuid ++ "_response",
   {ok, Shell} = radish:start_shell(RequestPort, ResponsePort),
   {ok, Socket} = gen_udp:open(0, [local]),
   handle_port_for_shell(RequestPort, ResponsePort, Socket, Shell).

handle_port_for_shell(RequestPort, ResponsePort, Socket, Shell) ->
   {ok, {_, _, Data}} = gen_udp:recv(RequestPort, 0),
   Response = radish_shell:run_command(Shell, erlang:iolist_to_binary(Data)),
   gen_udp:send(Socket, {local, ResponsePort}, 0, "r" ++ Response),
   handle_port_for_shell(RequestPort, ResponsePort, Socket, Shell).

supply_command(Command, Args, OutSocket) ->
   {ok, Socket} = gen_udp:open(0, [local]), % TODO: store this
   gen_udp:send(Socket, {local, OutSocket}, 0, "c" ++ Command),

   SendArg = fun(Arg) -> gen_udp:send(Socket, {local, OutSocket}, 0, "a" ++ Arg) end,
   lists:foreach(SendArg, Args).

finish_command(RequestPort, OutSocket) ->
   {ok, Socket} = gen_udp:open(0, [local]), % TODO: store this

   gen_udp:send(Socket, {local, OutSocket}, 0, "e"),
   {ok, _} = gen_udp:recv(RequestPort, 0).
