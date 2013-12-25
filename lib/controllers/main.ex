defmodule ApathyDrive.Main do

  use Weber.Controller

  layout false
  def action([], _conn) do
    {:render, [], []}
  end

  def websocket_init(pid, conn) do
    Players.connected(pid)
  end

  def websocket_message(pid, message, conn) do
    [{event, [message]}] = JSON.parse(message)
    IO.puts "Message! Pid: #{inspect pid}, Event: #{inspect event} Message: #{inspect message}"
    {_label, text} = message
    case event do
      "command" ->
        player = Players.find_by_connection(pid)
        Systems.Command.execute(player, text)
    end
  end

  def websocket_terminate(pid, conn) do
    Players.disconnected(pid)
  end

end
