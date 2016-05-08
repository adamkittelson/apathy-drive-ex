defmodule ApathyDrive.Unity do
  use GenServer
  require Logger
  alias ApathyDrive.Mobile

  @interval 60_000

  def start_link do
    GenServer.start_link(__MODULE__, %{"evil" => nil, "good" => nil}, name: __MODULE__)
  end

  def init(average_essences) do
    {:ok, average_essences}
  end

  def contribute(contributor, unity, essence) do
    GenServer.cast(__MODULE__, {:contribute, contributor, unity, essence})
  end

  def handle_cast({:contribute, contributor, unity, essence}, average_essences) when unity in ["evil", "good"] do
    average_essences = update_in average_essences, [unity], &(div((&1 || essence) + essence, 2))
    Mobile.average_essence(contributor, average_essences[unity])
    {:noreply, average_essences}
  end

end
