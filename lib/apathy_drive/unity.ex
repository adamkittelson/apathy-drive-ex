defmodule ApathyDrive.Unity do
  use GenServer
  require Logger
  alias ApathyDrive.Mobile

  @interval 60_000

  def start_link do
    GenServer.start_link(__MODULE__, %{"evil" => %{}, "good" => %{}}, name: __MODULE__)
  end

  def init(contributions) do
    Process.send_after(self, :redistribute_essence, @interval)

    {:ok, contributions}
  end

  def contribute(contributor, unity, contribution) do
    GenServer.cast(__MODULE__, {:contribute, contributor, unity, contribution})
  end

  def handle_cast({:contribute, contributor, unity, contribution}, contributions) when unity in ["evil", "good"] do
    contributions = update_in contributions, [unity, contributor], &((&1 || 0) + contribution)
    {:noreply, contributions}
  end

  def handle_info(:redistribute_essence, contributions) do
    Enum.each(contributions, fn({unity, unity_contributions}) ->
      Logger.debug "Redstributing Essence for #{unity}"

     if Enum.any?(unity_contributions) do
       unity_contributions
       |> calculate_distributions()
       |> distribute()
     end
    end)

    Process.send_after(self, :redistribute_essence, @interval)

    {:noreply, %{"evil" => %{}, "good" => %{}}}
  end

  def calculate_distributions(contributions) do
    exp_pool =
      contributions
      |> Map.values
      |> Enum.sum

    contributions
    |> Map.keys
    |> Enum.sort_by(&Map.get(contributions, &1))
    |> calculate_distributions(contributions, exp_pool)
  end

  defp calculate_distributions([], distributions, _exp_pool), do: distributions
  defp calculate_distributions([contributor | rest] = contributors, contributions, exp_pool) do
    contribution = contributions[contributor]

    share = min(contribution * 2, div(exp_pool, length(contributors)))

    difference = share - contribution

    contributions = Map.put(contributions, contributor, difference)

    calculate_distributions(rest, contributions, exp_pool - share)
  end

  defp distribute(distributions) do
    Enum.each(distributions, fn({member, amount}) ->
      Mobile.add_experience(member, amount)
    end)
  end
end
