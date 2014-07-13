defmodule Components.Mana do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Mana, :value)
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_mana, new_value})
  end

  def add(entity, amount) do
    GenEvent.notify(entity, {:add_mana, amount, Systems.Mana.max(entity)})
  end

  def subtract(entity, amount) do
    GenEvent.call(entity, Components.Mana, {:subtract, amount})
  end

  def serialize(entity) do
    %{"Mana" => value(entity)}
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, value) do
    {:ok, value, value}
  end

  def handle_call({:subtract, amount}, value) do
    if value >= amount do
      new_value = value - amount
      {:ok, true, new_value}
    else
      {:ok, false, value}
    end
  end

  def handle_event({:set_mana, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event({:add_mana, amount, max}, value) do
    {:ok, Enum.min([value + amount, max]) }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
