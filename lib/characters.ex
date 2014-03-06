defmodule Characters do
  use GenServer.Behaviour

  # Public API
  def add(character) do
    :gen_server.cast(:characters, {:add, character})
  end

  def all do
    :gen_server.call(:characters, :all)
  end

  def name_taken?(name) do
    Enum.any?(all, fn(character) ->
      Components.Name.get_name(character) == name
    end)
  end

  # GenServer API
  def start_link() do
    :gen_server.start_link({:local, :characters}, __MODULE__, [], [])
  end

  def init([]) do
    {:ok, []}
  end

  def handle_cast({:add, character}, characters) do
    {:noreply, [character | characters] }
  end

  def handle_call(:all, _from, characters) do
    {:reply, characters, characters}
  end

end
