defmodule ApathyDrive.Factions do
  use GenServer
  import Ecto.Query

  def start_link do
    GenServer.start_link(__MODULE__, %{bonus_pool: 0, timers: %{}}, name: __MODULE__)
  end

  def init(state) do
    state = state
            |> TimerManager.call_every({:reward, 3600000, fn -> send(self, :reward_factions) end})

    {:ok, state}
  end

  def add_to_bonus_pool(exp) do
    GenServer.cast(__MODULE__, {:add_to_bonus_pool, exp})
  end

  def faction_bonus(0, bonus_pool) do
    div(bonus_pool, 2)
  end
  def faction_bonus(1, bonus_pool) do
    div(bonus_pool, 3)
  end
  def faction_bonus(2, bonus_pool) do
    div(bonus_pool, 6)
  end

  def lair_counts do
    counts =
      from(r in Room, where: not is_nil(r.lair_faction),
                    group_by: r.lair_faction,
                    select: {r.lair_faction, count(r.id)})
      |> ApathyDrive.Repo.all

     ["Angel", "Demon", "Elemental"]
     |> Enum.reduce(%{}, fn(faction, factions) ->
          {_, count} = Enum.find(counts, {faction, 0}, fn({counted_faction, _}) -> faction == counted_faction end)
          Map.put(factions, faction, count)
        end)
  end

  def handle_cast({:add_to_bonus_pool, exp}, state) do
    {:noreply, update_in(state, [:bonus_pool], &(&1 + exp))}
  end

  def handle_info(:reward_factions, state) do
    counts = lair_counts

    indexed =
      counts
      |> Enum.sort_by(fn({_, count}) -> -count end)
      |> Enum.with_index

    ApathyDrive.Endpoint.broadcast! "spirits:online", "scroll", %{:html => "<p><br>       <span class='dark-yellow'>War Status</span></p>"}
    ApathyDrive.Endpoint.broadcast! "spirits:online", "scroll", %{:html => "<p><span class='dark-cyan'>Faction          Lairs</span></p>"}
    ApathyDrive.Endpoint.broadcast! "spirits:online", "scroll", %{:html => "<p><span class='dark-green'>======================</span></p>"}

    indexed
    |> Enum.each(fn({{faction, _count}, _index}) ->
         color = case faction do
           "Angel" ->
             "white"
           "Demon" ->
              "magenta"
           "Elemental" ->
              "dark-cyan"
         end
         ApathyDrive.Endpoint.broadcast! "spirits:online", "scroll", %{:html => "<p><span class='#{color}'>#{String.ljust("#{faction}s", 11)}#{String.rjust(inspect(counts[faction]), 11)}</span></p>"}
       end)

    indexed
    |> Enum.each(fn({{faction, count}, index}) ->
         faction_bonus = faction_bonus(index, state.bonus_pool)
         ApathyDrive.PubSub.broadcast!("spirits:#{faction}", {:lair_control_reward, count, faction_bonus})
       end)

    {:noreply, Map.put(state, :bonus_pool, 0)}
  end

  def handle_info({:timeout, _ref, {name, time, function}}, %{timers: timers} = state) do
    jitter = trunc(time / 2) + :random.uniform(time)

    new_ref = :erlang.start_timer(jitter, self, {name, time, function})

    timers = Map.put(timers, name, new_ref)

    TimerManager.execute_function(function)

    {:noreply, Map.put(state, :timers, timers)}
  end

  def handle_info({:timeout, _ref, {name, function}}, %{timers: timers} = state) do
    TimerManager.execute_function(function)

    timers = Map.delete(timers, name)

    {:noreply, Map.put(state, :timers, timers)}
  end
end
