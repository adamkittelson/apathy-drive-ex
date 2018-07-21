defmodule ApathyDrive.Gossip do
  require Logger
  alias ApathyDrive.Directory

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
  end

  def player_sign_out(game_name, player_name) do
    ApathyDriveWeb.Endpoint.broadcast!("mud:play", "scroll", %{
      html:
        "<p>#{ApathyDrive.Character.sanitize(player_name)} just left the distant Realm of #{
          ApathyDrive.Character.sanitize(game_name)
        }.</p>"
    })
  end

  def players do
    Directory.list_characters()
  end

  def user_agent do
    "Apathy Drive v#{to_string(Application.spec(:apathy_drive, :vsn))}"
  end
end
