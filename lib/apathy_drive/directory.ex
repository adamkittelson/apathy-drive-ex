defmodule ApathyDrive.Directory do
  use GenServer
  alias ApathyDrive.Match

  def start_link do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  def init(_state) do
    {:ok,
     %{
       local:
         %{
           # name => %{name: name, room: room_id, ref: ref}
         }
     }}
  end

  def list_characters do
    GenServer.call(__MODULE__, :list_characters)
  end

  def add_character(%{name: _name, room: _socket, ref: _ref} = character) do
    GenServer.call(__MODULE__, {:add_character, character})
  end

  def remove_character(name) do
    GenServer.call(__MODULE__, {:remove_character, name})
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

  def handle_call({:find, character}, _from, state) do
    state.local
    |> Map.values()
    |> Match.one(:name_starts_with, character)
    |> case do
      nil ->
        {:reply, :not_found, state}

      %{name: name, room: room_id, ref: ref} ->
        {:reply, {:local, name, room_id, ref}, state}
    end
  end

  def handle_call(:list_characters, _from, state) do
    {:reply, Map.keys(state.local), state}
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
end
