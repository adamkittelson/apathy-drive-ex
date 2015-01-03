defmodule Components.TimerManager do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.TimerManager, :get_timer_manager)
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_timer_manager, new_value})
  end

  def call_after(entity, {name, time, function}) do
    entity
    |> value
    |> TimerManager.call_after {name, time, function}
  end

  def call_every(entity, {name, time, function}) do
    entity
    |> value
    |> TimerManager.call_every {name, time, function}
  end

  def serialize(_entity) do
    nil
  end

  ### GenEvent API
  def init(name) do
    {:ok, name}
  end

  def handle_call(:get_timer_manager, timer_manager) do
    {:ok, timer_manager, timer_manager}
  end

  def handle_event({:set_timer_manager, new_timer_manager}, _timer_manager) do
    {:ok, new_timer_manager}
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
