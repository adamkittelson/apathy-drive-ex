defmodule ApathyDrive.Ticks do
  use Systems.Reload
  use Timex
  alias Phoenix.PubSub


  def start_link do
    {:ok, tm} = TimerManager.start_link

    TimerManager.call_every(tm, {:idle, 1_000, &idle/0})
    TimerManager.call_every(tm, {:hints, 60_000, &hints/0})
    TimerManager.call_every(tm, {:lair_spawning, 60_000, &lair_spawning/0})

    {:ok, tm}
  end

  def idle do
    PubSub.broadcast("spirits:online", :increment_idle)
  end

  def hints do
    PubSub.broadcast("spirits:hints",  :display_hint)
  end

  def lair_spawning do
    PubSub.broadcast("rooms:lairs",  {:spawn_monsters, Date.now |> Date.convert(:secs)})
  end
end
