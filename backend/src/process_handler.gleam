import gleam/erlang/process.{type Pid, type Subject}
import gleam/list
import gleam/otp/actor
import gleam/otp/port.{type Port}
import gleam/result

type SocketResponse =
  BitArray

type ProcessHandlerInfo {
  ProcessHandlerInfo(
    request_port: Port,
    request_buildup: List(#(Pid, SocketResponse)),
  )
}

pub fn new(port: Port) -> Result(Subject(Message), actor.StartError) {
  actor.start(ProcessHandlerInfo(port, []), handle_message)
}

pub fn recv(state: Subject(Message), pid: Pid) -> SocketResponse {
  actor.call(state, Recv(pid, _), 1_000_000)
}

pub fn close(state: Subject(Message)) -> Nil {
  actor.send(state, Kill)
}

pub type Message {
  Recv(Pid, reply_with: Subject(SocketResponse))
  Kill
}

fn handle_message(
  message: Message,
  state: ProcessHandlerInfo,
) -> actor.Next(Message, ProcessHandlerInfo) {
  case message {
    Recv(pid, client) -> {
      let #(response, updated_stack) =
        state.request_buildup
        |> list.pop_map(fn(x) {
          case x.0 == pid {
            True -> Ok(x.1)
            False -> Error(Nil)
          }
        })
        |> result.lazy_unwrap(fn() {
          handle_recv(state.request_port, state.request_buildup, pid)
        })

      actor.send(client, response)
      actor.continue(
        ProcessHandlerInfo(..state, request_buildup: updated_stack),
      )
    }
    Kill -> actor.Stop(process.Normal)
  }
}

fn handle_recv(
  request_port: Port,
  stack: List(#(Pid, SocketResponse)),
  pid: Pid,
) -> #(SocketResponse, List(#(Pid, SocketResponse))) {
  case recv_from_port(request_port) {
    #(attempted_pid, msg) if attempted_pid == pid -> #(msg, stack)
    #(attempted_pid, msg) ->
      handle_recv(request_port, [#(attempted_pid, msg), ..stack], pid)
  }
}

@external(erlang, "process_handler_ffi", "recv_from_port")
fn recv_from_port(port: Port) -> #(Pid, BitArray)
