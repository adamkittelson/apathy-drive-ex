defmodule ApathyDrive.Gossip.Players do
  @behaviour Gossip.Client.Players

  alias ApathyDrive.Directory

  @impl true
  def player_sign_in(game_name, player_name) do
    Directory.add_character(game_name, player_name)
  end

  @impl true
  def player_sign_out(game_name, player_name) do
    Directory.remove_character(game_name, player_name)
  end

  @impl true
  def player_update(_game, _players), do: :ok
end
