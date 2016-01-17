defmodule ApathyDrive.Unity do
  use GenServer
  require Logger
  alias ApathyDrive.Mobile

  @redistribution_interval 60_000

  def start_link do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init(forms) do
    Process.send_after(self, :redistribute_essence, @redistribution_interval)

    {:ok, forms}
  end

  def handle_info(:redistribute_essence, forms) do
    Logger.debug "Redstributing Essence"
    contributors = members()
    contributions = contributions(contributors)

    contributions
    |> calculate_distributions()
    |> distribute()

    Process.send_after(self, :redistribute_essence, @redistribution_interval)

    {:noreply, forms}
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

  def members do
    ApathyDrive.PubSub.subscribers("unity")
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
      Logger.debug "#{Mobile.name(member)} essence changes by #{inspect amount}"
      adjust_essence(member, amount)
    end)
  end

  defp contributions(contributors) do
    contributors
    |> Enum.reduce(%{}, fn(member, contributions) ->
         contribution = div(Mobile.experience(member), 100)
         Logger.debug "#{Mobile.name(member)} contributes #{inspect contribution}"
         Map.put(contributions, member, contribution)
       end)
  end

  defp adjust_essence(_member, amount) when amount == 0, do: :noop
  defp adjust_essence(member, amount) when amount > 0 do
    Mobile.send_scroll(member, "<p><span class='yellow'>You receive #{amount} essence via your connection to the unity.</span></p>")
    Mobile.add_experience(member, amount)
  end
  defp adjust_essence(member, amount) when amount < 0 do
    Mobile.send_scroll(member, "<p><span class='yellow'>You sacrifice #{abs(amount)} essence for the betterment of the unity.</span></p>")
    Mobile.add_experience(member, amount)
  end


end