defmodule ApathyDrive.Unity do
  use GenServer
  require Logger
  alias ApathyDrive.{Mobile, World}

  @interval 60_000

  def start_link do
    GenServer.start_link(__MODULE__, ["demon", "angel"], name: __MODULE__)
  end

  def init(unities) do
    #Process.send_after(self, :redistribute_essence, @interval)

    {:ok, unities}
  end

  def handle_info(:redistribute_essence, unities) do
    Enum.each(unities, fn(unity) ->
      Logger.debug "Redstributing Essence for #{unity}"
      mobile_contributors = ApathyDrive.PubSub.subscribers("#{unity}-unity:mobiles")
      room_contributors = ApathyDrive.PubSub.subscribers("#{unity}-unity:rooms")

     contributions = contributions(mobile_contributors, room_contributors)

     if Enum.any?(contributions) do
       contributions
       |> calculate_distributions()
       |> distribute()

       average =
         contributions
         |> Map.values
         |> Enum.map(&elem(&1, 1))
         |> Enum.sum
         |> div(map_size(contributions))

       World.set_average_essence(unity, average)
     end
    end)

    Process.send_after(self, :redistribute_essence, @interval)

    {:noreply, unities}
  end

  def calculate_distributions(contributions) do
    exp_pool =
      contributions
      |> Map.values
      |> Enum.map(&elem(&1, 1))
      |> Enum.sum

    contributions
    |> Map.keys
    |> Enum.sort_by(&Map.get(contributions, &1))
    |> calculate_distributions(contributions, exp_pool)
  end

  defp calculate_distributions([], distributions, _exp_pool), do: distributions
  defp calculate_distributions([contributor | rest] = contributors, contributions, exp_pool) do
    {entity_type, contribution} = contributions[contributor]
    share = min(contribution * 2, div(exp_pool, length(contributors)))

    difference = share - contribution

    contributions = Map.put(contributions, contributor, {entity_type, difference})

    calculate_distributions(rest, contributions, exp_pool - share)
  end

  defp distribute(distributions) do
    Enum.each(distributions, fn
      {member, {:mobile, amount} = distribution} ->
        entity = World.mobile(member)

        Logger.debug "#{entity.name} essence changes by #{inspect amount}"
        adjust_essence(member, distribution)
      {member, {:room, amount} = distribution} ->
        entity = World.room(member)

        Logger.debug "#{entity.name} essence changes by #{inspect amount}"
        adjust_essence(member, distribution)
    end)
  end

  defp contributions(mobiles, rooms) do
    mobile_contributions =
      mobiles
      |> Enum.reduce(%{}, fn(member, contributions) ->
           entity = World.mobile(member)

           essence = entity.experience || entity.spirit.experience
           contribution = div(essence, 100)
           Logger.debug "#{entity.name} contributes #{inspect contribution}"
           Map.put(contributions, member, {:mobile, contribution})
         end)

    rooms
    |> Enum.reduce(mobile_contributions, fn(member, contributions) ->
         entity = World.room(member)

         essence = Room.essence(entity)
         contribution = div(essence, 100)
         Logger.debug "#{entity.name} contributes #{inspect contribution}"
         Map.put(contributions, member, {:room, contribution})
       end)
  end

  defp adjust_essence(_member, {_entity_type, amount}) when amount == 0, do: :noop
  defp adjust_essence(member, {:mobile, amount}) when amount > 0 do
    Mobile.send_scroll(member, "<p>[<span class='yellow'>unity</span>]: You receive #{amount} essence.</p>")
    Mobile.add_experience(member, amount)
  end
  defp adjust_essence(member, {:room, amount}) when amount > 0 do
    send(member, {:add_essence, amount})
  end
  defp adjust_essence(member, {:mobile, amount}) when amount < 0 do
    Mobile.send_scroll(member, "<p>[<span class='yellow'>unity</span>]: You contribute #{abs(amount)} essence.</p>")
    Mobile.add_experience(member, amount)
  end
  defp adjust_essence(member, {:room, amount}) when amount < 0 do
    send(member, {:add_essence, amount})
  end

end