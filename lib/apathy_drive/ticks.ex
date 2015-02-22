defmodule ApathyDrive.Ticks do
  use Systems.Reload
  use Timex
  use GenServer
  alias Phoenix.PubSub

  def start_link(state) do
    GenServer.start_link(__MODULE__, state)
  end

  def init(state) do
    IO.puts "tick state: #{inspect state}"

    state = state
            |> TimerManager.call_every({:idle, 1_000, &idle/0})
            |> TimerManager.call_every({:hints, 60_000, &hints/0})
            |> TimerManager.call_every({:monster_spawning, 60_000, &monster_spawning/0})

    {:ok, state}
  end

  def idle do
    PubSub.broadcast("spirits:online", :increment_idle)
  end

  def hints do
    PubSub.broadcast("spirits:hints",  :display_hint)
  end

  def monster_spawning do
    PubSub.broadcast("rooms:lairs", {:spawn_monsters, Date.now |> Date.convert(:secs)})
    PubSub.broadcast("rooms:permanent_npcs", :spawn_permanent_npc)
  end
end
