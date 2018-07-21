defmodule ApathyDrive.Gossip do
  require Logger
  alias ApathyDrive.{Character, Directory, RoomServer}

  @behaviour Gossip.Client

  def channels do
    ["gossip"]
  end

  def message_broadcast(payload) do
    ApathyDriveWeb.Endpoint.broadcast!("chat:#{payload.channel}", "scroll", %{
      html:
        "<p>[<span class='dark-magenta'>gossip</span> : #{
          ApathyDrive.Character.sanitize(payload.name)
        }@#{ApathyDrive.Character.sanitize(payload.game)}] #{
          ApathyDrive.Character.sanitize(payload.message)
        }</p>"
    })
  end

  def player_sign_in(game_name, player_name) do
    ApathyDriveWeb.Endpoint.broadcast!("mud:play", "scroll", %{
      html:
        "<p>#{ApathyDrive.Character.sanitize(player_name)} just entered the distant Realm of #{
          ApathyDrive.Character.sanitize(game_name)
        }.</p>"
    })

    Directory.add_character(game_name, player_name)
  end

  def player_sign_out(game_name, player_name) do
    ApathyDriveWeb.Endpoint.broadcast!("mud:play", "scroll", %{
      html:
        "<p>#{ApathyDrive.Character.sanitize(player_name)} just left the distant Realm of #{
          ApathyDrive.Character.sanitize(game_name)
        }.</p>"
    })

    Directory.remove_character(game_name, player_name)
  end

  def players_status(game, players) do
    Enum.each(players, fn player ->
      Directory.add_character(Character.sanitize(game), Character.sanitize(player))
    end)
  end

  def players do
    Directory.list_characters()
    |> Enum.filter(& &1[:ref])
    |> Enum.map(& &1.name)
  end

  def tell_received(game_name, from_player, to_player, message) do
    case Directory.find(to_player) do
      {:local, _name, room, ref} ->
        from_player = Character.sanitize(from_player)
        game_name = Character.sanitize(game_name)

        room
        |> RoomServer.find()
        |> RoomServer.tell("#{from_player}@#{game_name}", ref, message)

      _ ->
        Logger.error(
          "received gossip tell from #{from_player}@#{game_name} to #{to_player} but they are not online"
        )
    end
  end

  def user_agent do
    "Apathy Drive v#{to_string(Application.spec(:apathy_drive, :vsn))}"
  end
end
