defmodule Players do
  use Systems.Reload
  use GenServer

  # Public API
  def connected(connection) do
    GenServer.cast(:players, {:connected, connection})
  end

  def disconnected(connection) do
    player = find_by_connection(connection)
    character = Components.Login.get_character(player)
    if character do
      Components.Online.value(character, false)
      Components.Player.value(character, nil)
    end
    GenServer.cast(:players, {:disconnected, player})
  end

  def send_message(player, message) do
    GenServer.cast(:players, {:send_message, player, message})
  end

  def find_by_connection(connection) do
    GenServer.call(:players, {:find_by_connection, connection})
  end

  def all do
    GenServer.call(:players, :all)
  end


  # GenServer API
  def start_link() do
    GenServer.start_link(Players, [], name: :players)
  end

  def init(players) do
    {:ok, players}
  end

  def handle_cast({:connected, connection}, players) do
    {:ok, player} = Entity.init

    Entity.add_component(player, Components.Connection, connection)

    Entity.add_component(player, Components.Login, nil)

    Components.Login.intro(player)

    {:noreply, [player | players] }
  end

  def handle_cast({:disconnected, player}, players) do
    {:noreply, List.delete(players, player)}
  end

  def handle_cast({:send_message, player, message}, players) do
    connection = Components.Connection.get_connection(player)
    if connection do
      send connection, Jazz.encode!(message)
    end
    {:noreply, players}
  end

  def handle_call({:find_by_connection, connection}, _from, players) do
    player = Enum.find players, fn (player) ->
      Components.Connection.get_connection(player) == connection
    end
    {:reply, player, players}
  end

  def handle_call(:all, _from, players) do
    {:reply, players, players}
  end

end
