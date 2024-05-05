-module(process_handler_ffi).
-export([recv_from_port/1]).

recv_from_port(RequestPort) ->
   {ok, {_, _, Data}} = gen_udp:recv(RequestPort, 0),
   {Pid, Message} = erlang:split_binary(erlang:iolist_to_binary(Data), 8),
   {erlang:list_to_pid(erlang:binary_to_list(Pid)), Message}.
