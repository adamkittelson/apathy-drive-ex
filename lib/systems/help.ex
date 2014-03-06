defmodule Systems.Help do
  use GenServer.Behaviour

  # Public API
  def add(entity) do
    :gen_server.cast(:help, {:add, entity})
  end

  def all do
    :gen_server.call(:help, :all)
  end

  def find(keyword) do
    help = find_by_name(keyword) || find_by_keyword(keyword)
    if help do
      help = Components.Help.get_help(help)
    end
    help
  end

  def find_by_name(keyword) do
    Enum.find(all, fn (entity) ->
      String.downcase(Components.Help.get_name(entity)) == String.downcase(keyword)
    end)
  end

  def find_by_keyword(keyword) do
    Enum.find(all, fn (entity) ->
      Components.Help.get_keywords(entity) |> Enum.member?(keyword)
    end)
  end

  # GenServer API
  def start_link() do
    :gen_server.start_link({:local, :help}, __MODULE__, [], [])
  end

  def init([]) do
    {:ok, []}
  end

  def handle_cast({:add, entity}, help) do
    {:noreply, [entity | help] }
  end

  def handle_call(:all, _from, help) do
    {:reply, help, help}
  end

end