defmodule ApathyDrive.Gossip.Core do
  require Logger
  alias ApathyDrive.{ChannelHistory, Directory, Repo}

  @behaviour Gossip.Client.Core

  @channel_colors %{
    "gossip" => "dark-magenta",
    "grapevine" => "magenta",
    "announce" => "yellow"
  }

  @impl true
  def authenticated do
    :noop
  end

  @impl true
  def user_agent do
    "Apathy Drive v#{to_string(Application.spec(:apathy_drive, :vsn))}"
  end

  @impl true
  def channels do
    ["gossip"]
  end

  @impl true
  def players do
    Directory.list_characters()
    |> Enum.filter(& &1[:ref])
    |> Enum.map(& &1.name)
  end

  @impl true
  def message_broadcast(payload) do
    channel = payload.channel

    channel = if channel == "gossip", do: "grapevine", else: channel

    message =
      "<p>[<span class='#{@channel_colors[channel]}'>#{channel}</span> : #{
        ApathyDrive.Character.sanitize(payload.name)
      }@#{ApathyDrive.Character.sanitize(payload.game)}] #{
        ApathyDrive.Character.sanitize(payload.message)
      }</p>"

    Repo.insert!(%ChannelHistory{
      character_name: ApathyDrive.Character.sanitize(payload.name),
      game_name: ApathyDrive.Character.sanitize(payload.game),
      channel_name: channel,
      message: message
    })

    ApathyDriveWeb.Endpoint.broadcast!("chat:#{payload.channel}", "chat", %{
      html: message
    })
  end
end
