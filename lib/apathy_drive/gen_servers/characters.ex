defmodule Characters do
  use Systems.Reload
  use GenServer

  # Public API
  def add(character, id: id) do
    GenServer.cast(:characters, {:add_id, character, id})
  end

  def add(character, url: url) do
    GenServer.cast(:characters, {:add_url, character, url})
  end

  def add(character, socket: pid) do
    GenServer.cast(:characters, {:add_socket, character, pid})
  end

  def remove(character) do
    GenServer.cast(:characters, {:remove_id,     Components.ID.value(character)})
    GenServer.cast(:characters, {:remove_url,    Components.URL.value(character)})
    socket = Components.Socket.value(character)
    if socket do
      GenServer.cast(:characters, {:remove_socket, socket.pid})
    end
  end

  def remove_socket(pid) do
    GenServer.cast(:characters, {:remove_socket, pid})
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
    GenServer.call(:characters, :online)
  end

  def find_by_id(id) do
    GenServer.call(:characters, {:id, id})
  end

  def find_by_url(url) do
    GenServer.call(:characters, {:url, url})
  end

  def find_by_socket(pid) do
    GenServer.call(:characters, {:socket, pid})
  end

  # GenServer API
  def start_link() do
    hd = HashDict.new
         |> put_in([:id],     HashDict.new)
         |> put_in([:url],    HashDict.new)
         |> put_in([:socket], HashDict.new)

    GenServer.start_link(Characters, hd, name: :characters)
  end

  def init(value) do
    {:ok, value}
  end

  def handle_cast({:add_id, character, id}, characters) do
    {:noreply, put_in(characters[:id][id], character) }
  end

  def handle_cast({:remove_id, id}, characters) do
    {:noreply, update_in(characters[:id], &(HashDict.delete(&1, id)))}
  end

  def handle_cast({:add_url, character, url}, characters) do
    {:noreply, put_in(characters[:url][url], character) }
  end

  def handle_cast({:remove_url, url}, characters) do
    {:noreply, update_in(characters[:url], &(HashDict.delete(&1, url)))}
  end

  def handle_cast({:add_socket, character, pid}, characters) do
    {:noreply, put_in(characters[:socket][pid], character) }
  end

  def handle_cast({:remove_socket, pid}, characters) do
    {:noreply, update_in(characters[:socket], &(HashDict.delete(&1, pid)))}
  end

  def handle_call(:all, _from, characters) do
    {:reply, characters, characters}
  end

  def handle_call({:url, url}, _from, characters) do
    {:reply, characters[:url][url], characters}
  end

  def handle_call({:id, id}, _from, characters) do
    {:reply, characters[:id][id], characters}
  end

  def handle_call({:socket, pid}, _from, characters) do
    {:reply, characters[:socket][pid], characters}
  end

  def handle_call(:online, _from, characters) do
    {:reply, characters[:socket] |> HashDict.values, characters}
  end

end
