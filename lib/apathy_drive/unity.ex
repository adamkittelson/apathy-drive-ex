defmodule ApathyDrive.Unity do
  use GenServer
  require Logger
  alias ApathyDrive.Mobile

  @interval 60_000

  def start_link do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def forms(mobile, limb) do
    GenServer.cast(__MODULE__, {:forms, mobile, limb})
  end

  def construct(mobile, mobile_forms, item_name) do
    GenServer.cast(__MODULE__, {:construct, mobile, mobile_forms, item_name})
  end

  def update_forms do
    GenServer.cast(__MODULE__, :update_forms)
  end

  def init(forms) do
    Process.send_after(self, :redistribute_essence, @interval)

    {:ok, forms}
  end

  def handle_cast({:forms, mobile, limb}, forms) do
    if mobile in members() do
      send mobile, {:list_forms, forms, limb}
    else
      send mobile, {:list_forms, :non_member, limb}
    end

    {:noreply, forms}
  end

  def handle_cast({:construct, mobile, mobile_forms, item_name}, forms) do
    match = if mobile in members() do
      find_form(forms, item_name)
    else
      find_form(mobile_forms, item_name)
    end

    send mobile, {:construct_item, item_name, match}

    {:noreply, forms}
  end

  def handle_cast(:update_forms, _forms) do
    :timer.sleep(100)

    forms =
      members()
      |> Enum.map(&Mobile.forms/1)
      |> List.flatten
      |> Enum.uniq

    {:noreply, forms}
  end

  def handle_info(:redistribute_essence, forms) do
    Logger.debug "Redstributing Essence"
    contributors = members()
    contributions = contributions(contributors)

    contributions
    |> calculate_distributions()
    |> distribute()

    Process.send_after(self, :redistribute_essence, @interval)

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

  defp find_form(forms, item_name) do
    forms
    |> Enum.map(&(%{name: &1.name, keywords: String.split(&1.name), item: &1}))
    |> Systems.Match.one(:name_contains, item_name)
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