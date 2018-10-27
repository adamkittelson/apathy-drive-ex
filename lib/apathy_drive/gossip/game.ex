defmodule ApathyDrive.Gossip.Games do
  @behaviour Gossip.Client.Games

  @impl true
  def game_update(_game), do: :ok

  @impl true
  def game_connect(_game), do: :ok

  @impl true
  def game_disconnect(_game), do: :ok
end
