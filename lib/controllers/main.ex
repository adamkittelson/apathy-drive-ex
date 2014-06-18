defmodule ApathyDrive.Main do
  use Systems.Reload
  use Weber.Controller

  layout false
  def home(_, _conn) do
    {:render, [], []}
  end

  def game([id: id], _conn) do
    {:render, [], []}
  end

  def game(_, _conn) do
    url = Systems.Login.create
    {:redirect, "/game/#{url}"}
  end

  def websocket_init(pid, _conn) do
    Players.connected(pid)
  end

  def websocket_message(pid, message, _conn) do
    player             = Players.find_by_connection(pid)
    [{event, message}] = Jazz.decode!(message) |> Map.to_list
    case event do
      "login" ->
        Systems.Login.login(player, message)
      "command" ->
        [command | arguments] = String.split(message)
        Systems.Command.execute(player, command, arguments)
    end
  end

  def websocket_terminate(pid, _conn) do
    Players.disconnected(pid)
  end

end
