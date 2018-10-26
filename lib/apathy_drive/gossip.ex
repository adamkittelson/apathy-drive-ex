defmodule ApathyDrive.Gossip do
  require Logger
  alias ApathyDrive.{ChannelHistory, Character, Directory, Repo, RoomServer}

  @behaviour Gossip.Client

  @channel_colors %{
    "gossip" => "dark-magenta",
    "announce" => "yellow"
  }

  def channels do
    ["gossip"]
  end

  def message_broadcast(payload) do
    message =
      "<p>[<span class='#{@channel_colors[payload.channel]}'>#{payload.channel}</span> : #{
        ApathyDrive.Character.sanitize(payload.name)
      }@#{ApathyDrive.Character.sanitize(payload.game)}] #{
        ApathyDrive.Character.sanitize(payload.message)
      }</p>"

    Repo.insert!(%ChannelHistory{
      character_name: ApathyDrive.Character.sanitize(payload.name),
      game_name: ApathyDrive.Character.sanitize(payload.game),
      channel_name: payload.channel,
      message: message
    })

    ApathyDriveWeb.Endpoint.broadcast!("chat:#{payload.channel}", "chat", %{
      html: message
    })
  end

  def player_sign_in(game_name, player_name) do
    message =
      "<p>#{ApathyDrive.Character.sanitize(player_name)} just entered the distant Realm of #{
        ApathyDrive.Character.sanitize(game_name)
      }.</p>"

    Repo.insert!(%ChannelHistory{
      character_name: ApathyDrive.Character.sanitize(player_name),
      game_name: ApathyDrive.Character.sanitize(game_name),
      message: message
    })

    ApathyDriveWeb.Endpoint.broadcast!("mud:play", "chat", %{
      html: message
    })

    Directory.add_character(game_name, player_name)
  end

  def player_sign_out(game_name, player_name) do
    message =
      "<p>#{ApathyDrive.Character.sanitize(player_name)} just left the distant Realm of #{
        ApathyDrive.Character.sanitize(game_name)
      }.</p>"

    Repo.insert!(%ChannelHistory{
      character_name: ApathyDrive.Character.sanitize(player_name),
      game_name: ApathyDrive.Character.sanitize(game_name),
      message: message
    })

    ApathyDriveWeb.Endpoint.broadcast!("mud:play", "chat", %{
      html: message
    })

    Directory.remove_character(game_name, player_name)
  end

  def players_status(game, players) do
    Directory.update_remote_players(game, players)
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
