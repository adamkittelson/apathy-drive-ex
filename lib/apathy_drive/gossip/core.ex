defmodule ApathyDrive.Gossip.Core do
  require Logger
  alias ApathyDrive.Directory

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

    name = String.replace(payload.name, ~r/ \(.* in I3\)/, "")

    message =
      "<p>[<span class='#{@channel_colors[channel]}'>#{channel}</span> : #{ApathyDrive.Character.sanitize(name)}@#{ApathyDrive.Character.sanitize(payload.game)}] #{ApathyDrive.Character.sanitize(payload.message)}</p>"

    ApathyDriveWeb.Endpoint.broadcast!("chat:#{payload.channel}", "chat", %{
      html: message,
      chat_tab: "chat"
    })
  end
end
