defmodule Players do
  use GenServer.Behaviour

  # Public API
  def connected(connection) do
    IO.puts "Connected! pid: #{inspect connection}"
    :gen_server.cast(:players, {:connected, connection})
  end

  def disconnected(connection) do
    player = find_by_connection(connection)
    IO.puts "Player Disconnected! Pid: #{inspect connection}"
    :gen_server.cast(:players, {:disconnected, player})
  end

  def send_message(player, message) do
    :gen_server.cast(:players, {:send_message, player, message})
  end

  def find_by_connection(connection) do
    :gen_server.call(:players, {:find_by_connection, connection})
  end


  # GenServer API
  def start_link() do
    :gen_server.start_link({:local, :players}, __MODULE__, [], [])
  end

  def init([]) do
    {:ok, []}
  end

  def handle_cast({:connected, connection}, players) do
    {:ok, player} = ApathyDrive.Entity.init

    ApathyDrive.Entity.add_component(player, Components.Connection, connection)

    room_to_start_in = :global.whereis_name(:"82325")
    ApathyDrive.Entity.add_component(player, Components.CurrentRoom, room_to_start_in)

    Systems.Room.display_current_room(player)

    {:noreply, [player | players] }
  end

  def handle_cast({:disconnected, player}, players) do
    {:noreply, List.delete(players, player)}
  end

  def handle_cast({:send_message, player, message}, players) do
    connection = Components.Connection.get_connection(player)
    connection <- JSON.generate(message)
    {:noreply, players}
  end

  def handle_call({:find_by_connection, connection}, _from, players) do
    player = Enum.find players, fn (player) ->
      Components.Connection.get_connection(player) == connection
    end
    IO.puts "Found player #{inspect player} by connection #{inspect connection}"
    {:reply, player, players}
  end

end