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

  def lair_counts do
    from(r in Room, where: not is_nil(r.lair_faction),
                    group_by: r.lair_faction,
                    select: {r.lair_faction, count(r.id)})
    |> ApathyDrive.Repo.all
  end

  def handle_cast({:add_to_bonus_pool, exp}, state) do
    {:noreply, update_in(state, [:bonus_pool], &(&1 + exp))}
  end

  def handle_info(:reward_factions, state) do
    counts = lair_counts

    counts
    |> Enum.each(fn({faction, count}) ->
         ApathyDrive.PubSub.broadcast!("spirits:#{faction}", {:lair_control_reward, count})
       end)

    {winning_faction, _count} =
      counts
      |> Enum.sort_by(fn({_, count}) -> -count end)
      |> hd

    winner_count =
      ApathyDrive.PubSub.subscribers("spirits:#{winning_faction}")
      |> length

    if winner_count > 0 do
      ApathyDrive.PubSub.broadcast!("spirits:#{winning_faction}", {:lair_control_victory_reward, div(state.bonus_pool, winner_count)})
      {:noreply, Map.put(state, :bonus_pool, 0)}
    else
      {:noreply, state}
    end
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
