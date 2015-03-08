defmodule ApathyDrive.Ticks do

  use Timex
  use GenServer
  alias Phoenix.PubSub

  def start_link(state) do
    GenServer.start_link(__MODULE__, state)
  end

  def init(state) do
    state = state
            |> TimerManager.call_every({:idle, 1_000, &idle/0})
            |> TimerManager.call_every({:hints, 60_000, &hints/0})
            |> TimerManager.call_every({:spawning, 60_000, &spawning/0})
            |> TimerManager.call_every({:monster_regen, 5_000, &monster_regen/0})

    {:ok, state}
  end

  def idle do
    PubSub.broadcast!(ApathyDrive.PubSub, "spirits:online", :increment_idle)
  end

  def hints do
    PubSub.broadcast!(ApathyDrive.PubSub, "spirits:hints",  :display_hint)
  end

  def spawning do
    PubSub.broadcast!(ApathyDrive.PubSub, "rooms:lairs", {:spawn_monsters, Date.now |> Date.convert(:secs)})
    PubSub.broadcast!(ApathyDrive.PubSub, "rooms:permanent_npcs", :spawn_permanent_npc)
    PubSub.broadcast!(ApathyDrive.PubSub, "rooms:placed_items", :spawn_placed_items)
  end

  def monster_regen do
    PubSub.broadcast!(ApathyDrive.PubSub, "monsters", :regen)
  end

  def handle_info({:timeout, _ref, {name, time, function}}, %{timers: timers} = state) do
    new_ref = :erlang.start_timer(time, self, {name, time, function})

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
