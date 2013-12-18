defmodule ApathyDrive.Main do

  use Weber.Controller

  layout false
  def action([], _conn) do
    {:render, [], []}
  end

  def websocket_init(pid, conn) do
    IO.puts "Connected! pid: #{inspect pid}"
  end

  def websocket_message(pid, message, conn) do
    [{event, [message]}] = JSON.parse(message)
    IO.puts "Message! Pid: #{inspect pid}, Event: #{inspect event} Message: #{inspect message}"
    {_label, text} = message
    case event do
      "command" ->
        pid <- "You said #{text}."
    end
  end

  def websocket_terminate(pid, conn) do
    IO.puts "Terminated! Pid: #{inspect pid}"
  end

end
