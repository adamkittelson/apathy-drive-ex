defmodule ApathyDrive.Directory do
  use GenServer
  alias ApathyDrive.Match

  def start_link do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  def init(_state) do
    Process.send_after(self(), :refresh_remote_players, :timer.seconds(5))

    {:ok,
     %{
       local:
         %{
           # name => %{name: name, room: room_id, ref: ref}
         },
       # %{name: name, game: game}
       remote: MapSet.new()
     }}
  end

  def list_characters do
    GenServer.call(__MODULE__, :list_characters)
  end

  def add_character(%{name: _name, room: _socket, ref: _ref} = character) do
    GenServer.call(__MODULE__, {:add_character, character})
  end

  def add_character(game, character) do
    GenServer.call(__MODULE__, {:add_character, game, character})
  end

  def remove_character(name) do
    GenServer.call(__MODULE__, {:remove_character, name})
  end

  def remove_character(game, name) do
    GenServer.call(__MODULE__, {:remove_character, game, name})
  end

  def find(name) do
    GenServer.call(__MODULE__, {:find, name})
  end

  def handle_call({:add_character, character}, _from, state) do
    if state.local[character.name] == nil do
      Gossip.player_sign_in(character.name)

      ApathyDriveWeb.Endpoint.broadcast!("mud:play", "scroll", %{
        html: "<p>#{character.name} just entered the Realm.</p>"
      })
    end

    {:reply, :ok, put_in(state, [:local, character.name], character)}
  end

  def handle_call({:add_character, game, name}, _from, state) do
    {:reply, :ok, update_in(state, [:remote], &MapSet.put(&1, %{name: name, game: game}))}
  end

  def handle_call({:find, character}, _from, state) do
    {character, game_name} =
      case String.split(character, "@") do
        [character] ->
          {character, nil}

        [character, game_name] ->
          {character, game_name}
      end

    characters =
      if game_name do
        Enum.into(state.remote, [])
      else
        all_characters(state)
      end

    characters
    |> Match.one(:name_starts_with, character)
    |> case do
      nil ->
        {:reply, :not_found, state}

      %{name: name, game: game} ->
        if String.starts_with?(String.downcase(game), String.downcase(game_name)) do
          {:reply, {:remote, name, game}, state}
        else
          {:reply, :not_found, state}
        end

      %{name: name, room: room_id, ref: ref} ->
        {:reply, {:local, name, room_id, ref}, state}
    end
  end

  def handle_call(:list_characters, _from, state) do
    {:reply, all_characters(state), state}
  end

  def handle_call({:remove_character, name}, _from, state) do
    if character = state.local[name] do
      Gossip.player_sign_out(character.name)

      ApathyDriveWeb.Endpoint.broadcast!("mud:play", "scroll", %{
        html: "<p>#{character.name} just left the Realm.</p>"
      })
    end

    {:reply, :ok, update_in(state.local, &Map.delete(&1, name))}
  end

  def handle_call({:remove_character, game, name}, _from, state) do
    {:reply, :ok, update_in(state.remote, &MapSet.delete(&1, %{name: name, game: game}))}
  end

  def handle_info(:refresh_remote_players, state) do
    Gossip.request_players_online()
    Process.send_after(self(), :refresh_remote_players, :timer.minutes(5))
    {:noreply, state}
  end

  defp all_characters(state) do
    local_players = Map.values(state.local)
    remote_players = Enum.into(state.remote, [])

    local_players ++ remote_players
  end
end
