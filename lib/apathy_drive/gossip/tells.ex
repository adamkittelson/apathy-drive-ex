defmodule ApathyDrive.Gossip.Tells do
  require Logger
  alias ApathyDrive.{Character, Directory, RoomServer}
  @behaviour Gossip.Client.Tells

  @impl true
  def tell_receive(game_name, from_player, to_player, message) do
    case Directory.find(to_player) do
      {:local, _name, room, ref} ->
        from_player = Character.sanitize(from_player)
        game_name = Character.sanitize(game_name)

        room
        |> RoomServer.find()
        |> RoomServer.tell(from_player, game_name, ref, message)

      _ ->
        Logger.error(
          "received gossip tell from #{from_player}@#{game_name} to #{to_player} but they are not online"
        )
    end
  end
end
