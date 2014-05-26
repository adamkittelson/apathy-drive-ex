defmodule Classes do
  use GenServer.Behaviour

  # Public API
  def add(class) do
    :gen_server.cast(:classes, {:add, class})
  end

  def all do
    :gen_server.call(:classes, :all)
  end

  def find_by_number(number) do
    Enum.find(all, fn (class) ->
      Components.Number.get_number(class) == number
    end)
  end

  # GenServer API
  def start_link() do
    :gen_server.start_link({:local, :classes}, __MODULE__, [], [])
  end

  def init([]) do
    {:ok, []}
  end

  def handle_cast({:add, class}, classes) do
    {:noreply, [class | classes] }
  end

  def handle_call(:all, _from, classes) do
    {:reply, classes, classes}
  end

end
