defmodule Characters do
  use Systems.Reload
  use GenServer.Behaviour

  # Public API
  def add(character) do
    :gen_server.cast(:characters, {:add, character})
  end

  def remove(character) do
    :gen_server.cast(:characters, {:remove, character})
  end

  def all do
    :gen_server.call(:characters, :all)
  end

  def name_taken?(name) do
    Enum.any?(all, fn(character) ->
      String.downcase(Components.Name.get_name(character)) == String.downcase(name)
    end)
  end

  def for_account(account) do
    Enum.filter(all, fn(character) ->
      Components.AccountID.value(character) == account.id
    end)
  end

  def online do
    all |> Enum.filter fn(character) -> Components.Online.value(character) == true end
  end

  def find_by_account_and_name(account, name) do
    for_account(account) |> Enum.find fn(character) ->
      String.downcase(Components.Name.get_name(character)) == String.downcase(name)
    end
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

  def handle_cast({:remove, character}, characters) do
    {:noreply, List.delete(character) }
  end

  def handle_call(:all, _from, characters) do
    {:reply, characters, characters}
  end

end
