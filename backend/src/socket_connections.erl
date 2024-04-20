-module(socket_connections).
-export([generate_server/0, read_stdin/1, send_stdout/2, identity/1]).

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
   {ok, StdinPort} = gen_udp:open(0, [{active, false}, {ifaddr, {local,"/tmp/radish/" ++ Uuid ++ "_stdin"}}]),
   ResponsePort = "/tmp/radish/" ++ Uuid ++ "_response",
   {ok, Shell} = radish:start_shell(StdinPort, ResponsePort),
   {ok, Socket} = gen_udp:open(0, [local]),
   handle_port_for_shell(RequestPort, ResponsePort, Socket, Shell).

handle_port_for_shell(RequestPort, ResponsePort, Socket, Shell) ->
   io:fwrite("readyyyy!!!~n"),
   {ok, {_, _, Data}} = gen_udp:recv(RequestPort, 0),
   io:fwrite("how coool!!!!!!!!~n"),
   Response = radish_shell:run_command(Shell, erlang:iolist_to_binary(Data)),
   gen_udp:send(Socket, {local, ResponsePort}, 0, "r" ++ Response),
   handle_port_for_shell(RequestPort, ResponsePort, Socket, Shell).

read_stdin(RequestPort) ->
   case gen_udp:recv(RequestPort, 0, 100) of
      {ok, {_, _, Data}} -> 
         Data;
      {error, _} -> 
         timer:sleep(100),
         read_stdin(RequestPort)
   end.

send_stdout(ResponseSocket, Contents) ->
   {ok, Socket} = gen_udp:open(0, [local]), % TODO: store this
   gen_udp:send(Socket, {local, ResponseSocket}, 0, "o" ++ Contents).

identity(X) ->
    X.
