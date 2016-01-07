defmodule ApathyDrive.Ticks do
  use Timex
  use GenServer
  alias ApathyDrive.{PubSub, TimerManager}

  def start_link(state) do
    GenServer.start_link(__MODULE__, state)
  end

  def init(state) do
    state = state
            |> TimerManager.call_every({:idle, 1_000, [__MODULE__, :idle, []]})
            |> TimerManager.call_every({:hints, 60_000, [__MODULE__, :hints, []]})

    {:ok, state}
  end

  def idle do
    PubSub.broadcast!("spirits:online", :increment_idle)
  end

  def hints do
    PubSub.broadcast!("spirits:hints",  :display_hint)
  end

  def handle_info({:timeout, _ref, {name, time, [module, function, args]}}, %{timers: timers} = state) do
    jitter = trunc(time / 2) + :random.uniform(time)

    new_ref = :erlang.start_timer(jitter, self, {name, time, [module, function, args]})

    timers = Map.put(timers, name, new_ref)

    apply module, function, args

    {:noreply, Map.put(state, :timers, timers)}
  end

  def handle_info({:timeout, _ref, {name, [module, function, args]}}, %{timers: timers} = state) do
    apply module, function, args

    timers = Map.delete(timers, name)

    {:noreply, Map.put(state, :timers, timers)}
  end
end
