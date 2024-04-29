-module(utils_ffi).

-export([simulate_shell/1, get_command/2, setup/0]).

simulate_shell(Uuid) ->
   RequestPortName = <<"/tmp/radish/", Uuid/binary, "_request">>,
   {ok, RequestPort} = gen_udp:open(0, [{active, false}, {ifaddr, {local,RequestPortName}}]),

   % usually this would be done on the frontend, but we simulate it here
   ResponsePortName = <<"/tmp/radish/", Uuid/binary, "_response">>,
   {ok, ResponsePort} = gen_udp:open(0, [{active, false}, {ifaddr, {local,ResponsePortName}}]),

   {RequestPort, ResponsePort, RequestPortName, ResponsePortName}.

get_command(RequestPortName, ResponsePort) ->
   case gen_udp:recv(ResponsePort, 0, 50) of
       {error, timeout} -> response;
       {ok, {_, _, "c" ++ Command}} ->
         {command, lists:reverse(handle_command(RequestPortName, ResponsePort, [{erlang:iolist_to_binary(Command), []}]))}
   end.

handle_command(RequestPortName, ResponsePort, Commands) ->
   {ok, {_, _, Data}} = gen_udp:recv(ResponsePort, 0),
   case Data of
       "e" ++ _End ->
           {ok, Socket} = gen_udp:open(0, [local]), % TODO: store this
           gen_udp:send(Socket, {local, RequestPortName}, 0, "end"),
           Commands;
       "a" ++ Arg ->
           [{Cmd, Args}|Rest] = Commands,
           handle_command(RequestPortName, ResponsePort, [{Cmd, [erlang:iolist_to_binary(Arg)|Args]}|Rest]);
       "c" ++ Command ->
           handle_command(RequestPortName, ResponsePort, [{erlang:iolist_to_binary(Command), []}|Commands])
   end.

setup() ->
   file:make_dir("/tmp/radish").
