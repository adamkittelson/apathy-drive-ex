defmodule ApathyDrive.Gossip.Core do
  require Logger
  alias ApathyDrive.Directory

  @behaviour Gossip.Client.Core

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
  def message_broadcast(_payload), do: :noop
end
