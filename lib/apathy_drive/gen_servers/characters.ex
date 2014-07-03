defmodule Characters do
  use Systems.Reload
  use GenServer

  # Public API
  def add(character) do
    GenServer.cast(:characters, {:add, character})
  end

  def remove(character) do
    GenServer.cast(:characters, {:remove, character})
  end

  def all do
    GenServer.call(:characters, :all)
  end

  def name_taken?(name) do
    Enum.any?(all, fn(character) ->
      String.downcase(Components.Name.get_name(character)) == String.downcase(name)
    end)
  end

  def online do
    all |> Enum.filter fn(character) -> Components.Online.value(character) == true end
  end

  def find_by_url(url) do
    Enum.find(all, &(Components.URL.value(&1) == url))
  end

  def find_by_socket(socket) do
    Components.all(Components.Socket)
    |> HashSet.to_list
    |> Enum.find(&(Components.Socket.value(&1).pid == socket.pid))
  end

  # GenServer API
  def start_link() do
    GenServer.start_link(Characters, [], name: :characters)
  end

  def init(value) do
    {:ok, value}
  end

  def handle_cast({:add, character}, characters) do
    {:noreply, [character | characters] }
  end

  def handle_cast({:remove, character}, _characters) do
    {:noreply, List.delete(character) }
  end

  def handle_call(:all, _from, characters) do
    {:reply, characters, characters}
  end

end
