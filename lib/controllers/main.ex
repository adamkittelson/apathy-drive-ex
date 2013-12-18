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
    IO.puts "Message! Pid: #{inspect pid}, Message: #{inspect message}"
  end

  def websocket_terminate(pid, conn) do
    IO.puts "Terminated! Pid: #{inspect pid}"
  end

end
