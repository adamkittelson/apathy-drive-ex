defmodule ApathyDrive.Ticks do
  use Systems.Reload
  alias Phoenix.PubSub


  def start_link do
    {:ok, tm} = TimerManager.start_link

    TimerManager.call_every(tm, {:idle, 1_000, &idle/0})
    TimerManager.call_every(tm, {:idle, 60_000, &hints/0})

    {:ok, tm}
  end

  def idle do
    PubSub.broadcast("spirits:online", :increment_idle)
  end

  def hints do
    PubSub.broadcast("spirits:hints",  :display_hint)
  end
end
