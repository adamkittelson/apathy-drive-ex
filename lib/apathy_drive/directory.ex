defmodule ApathyDrive.Directory do
  use GenServer

  def start_link do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  def init(state) do
    {:ok, state}
  end

  def list_characters do
    GenServer.call(__MODULE__, :list_characters)
  end

  def add_character(%{name: _name, ref: _ref} = character) do
    GenServer.call(__MODULE__, {:add_character, character})
  end

  def remove_character(ref) when is_reference(ref) do
    GenServer.call(__MODULE__, {:remove_character, ref})
  end

  def handle_call({:add_character, character}, _from, state) do
    Gossip.player_sign_in(character.name)

    ApathyDriveWeb.Endpoint.broadcast!("mud:play", "scroll", %{
      html: "<p>#{character.name} just entered the Realm.</p>"
    })

    {:reply, :ok, put_in(state, [character.ref], character.name)}
  end

  def handle_call(:list_characters, _from, state) do
    {:reply, Map.values(state), state}
  end

  def handle_call({:remove_character, ref}, _from, state) do
    if character = state[ref] do
      Gossip.player_sign_out(character)

      ApathyDriveWeb.Endpoint.broadcast!("mud:play", "scroll", %{
        html: "<p>#{character} just left the Realm.</p>"
      })
    end

    {:reply, :ok, Map.delete(state, ref)}
  end
end
