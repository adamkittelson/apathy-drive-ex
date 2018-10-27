defmodule ApathyDrive.Gossip.Players do
  @behaviour Gossip.Client.Players

  alias ApathyDrive.{ChannelHistory, Directory, Repo}

  @impl true
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

  @impl true
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

  @impl true
  def player_update(_game, _players), do: :ok
end
