defmodule Components.HP do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.HP, :value)
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_hp, new_value})
  end

  def add(entity, amount) do
    GenEvent.notify(entity, {:add_hp, amount, Systems.HP.max(entity)})
  end

  def subtract(entity, amount) do
    GenEvent.call(entity, Components.HP, {:subtract, amount})
  end

  def serialize(entity) do
    %{"HP" => value(entity)}
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, value) do
    {:ok, value, value}
  end

  def handle_event({:set_hp, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event({:add_hp, amount, max}, value) do
    {:ok, Enum.min([value + amount, max]) }
  end

  def handle_call({:subtract, amount}, value) do
    if value >= amount do
      new_value = value - amount
      {:ok, true, new_value}
    else
      {:ok, false, value}
    end
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
