defmodule Components.Combat do
  use Systems.Reload
  use GenEvent
  use Timex

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Combat, :value)
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_combat, new_value})
  end

  def in_combat?(entity) do
    GenEvent.call(entity, Components.Combat, :in_combat?)
  end

  def set_timer(entity, timer) do
    if value(entity)[:timer] do
      :timer.cancel(timer)
    else
      GenEvent.notify(entity, {:set_combat_timer, timer})
    end
  end

  def stop_timer(entity) do
    if value(entity)[:timer] do
      :timer.cancel(value(entity)[:timer])
      GenEvent.notify(entity, :stop_combat_timer)
    end
  end

  def set_break_at(entity) do
    GenEvent.notify(entity, :set_break_at)
  end

  def serialize(entity) do
    value = put_in(value(entity)["break_at"], Date.convert(value(entity)["break_at"], :secs))
    %{"Combat" => Map.delete(value, :timer)}
  end

  ### GenEvent API
  def init(value) do
    {:ok, put_in(value["break_at"], Date.from(value["break_at"], :secs))}
  end

  def handle_call(:value, value) do
    {:ok, value, value}
  end

  def handle_call(:in_combat?, value) do
    {:ok, !!value[:timer], value}
  end

  def handle_event({:set_combat, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event({:set_combat_timer, timer}, value) do
    {:ok, put_in(value[:timer], timer) }
  end

  def handle_event(:stop_combat_timer, value) do
    {:ok, Map.delete(value, :timer) }
  end

  def handle_event(:set_break_at, value) do
    value = put_in(value["break_at"], Date.shift(Date.now, secs: 5))
    {:ok, value}
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
