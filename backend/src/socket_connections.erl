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
   {ok, Socket} = gen_udp:open(0, [local]),
   ResponsePort = "/tmp/radish/" ++ Uuid ++ "_response",
   {{ok, Shell}, ProcessHandler} = radish:start_shell(RequestPort, ResponsePort),

   {ok, {_, _, Data}} = gen_udp:recv(RequestPort, 0),
   case Data of
       "i" ->
         Pid = erlang:pid_to_list(erlang:self()),
         gen_udp:send(Socket, {local, ResponsePort}, 0, Pid),
         handle_port_for_shell(ProcessHandler, ResponsePort, Socket, Shell);
       Fileconts ->
           Response = radish_shell:run_file(Shell, erlang:iolist_to_binary(Fileconts)),
           Pid = erlang:pid_to_list(erlang:self()),
           gen_udp:send(Socket, {local, ResponsePort}, 0, [Pid, "r", Response])
   end.


handle_port_for_shell(ProcessHandler, ResponsePort, Socket, Shell) ->
   Data = process_handler:recv(ProcessHandler, erlang:self()),
   Response = radish_shell:run_command(Shell, Data),

   Pid = erlang:pid_to_list(erlang:self()),
   gen_udp:send(Socket, {local, ResponsePort}, 0, [Pid, "r", Response]),
   handle_port_for_shell(ProcessHandler, ResponsePort, Socket, Shell).

supply_command(Command, Args, OutSocket) ->
   {ok, Socket} = gen_udp:open(0, [local]), % TODO: store this
   Pid = erlang:pid_to_list(erlang:self()),
   Data = [Pid, "c", Command],
   gen_udp:send(Socket, {local, OutSocket}, 0, Data),

   SendArg = fun(Arg) -> gen_udp:send(Socket, {local, OutSocket}, 0, [Pid, "a", Arg]) end,
   lists:foreach(SendArg, Args).

finish_command(ProcessHandler, OutSocket) ->
   {ok, Socket} = gen_udp:open(0, [local]), % TODO: store this
   Pid = erlang:pid_to_list(erlang:self()),

   gen_udp:send(Socket, {local, OutSocket}, 0, [Pid, "e"]),
   process_handler:recv(ProcessHandler, erlang:self()).
