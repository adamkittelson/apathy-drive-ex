defmodule ApathyDrive.Unity do
  use GenServer
  require Logger
  alias ApathyDrive.Mobile

  @interval 60_000

  def start_link do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init(unities) do
    send(self, :update_unities)
    Process.send_after(self, :redistribute_essence, @interval)

    {:ok, unities}
  end

  def handle_info(:update_unities, _unities) do
    {:noreply, Spirit.unities}
  end

  def handle_info(:redistribute_essence, unities) do
    Enum.each(unities, fn(unity) ->
      Logger.debug "Redstributing Essence for #{unity}"
      contributors = ApathyDrive.PubSub.subscribers("#{unity}-unity")
      contributions = contributions(contributors)

      contributions
      |> calculate_distributions()
      |> distribute()

    end)

    Process.send_after(self, :redistribute_essence, @interval)

    {:noreply, unities}
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
    Mobile.send_unity(member, "<span class='yellow'>You receive #{amount} essence via your connection to the unity.</span>")
    Mobile.add_experience(member, amount)
  end
  defp adjust_essence(member, amount) when amount < 0 do
    Mobile.send_unity(member, "<span class='yellow'>You sacrifice #{abs(amount)} essence for the betterment of the unity.</span>")
    Mobile.add_experience(member, amount)
  end


end