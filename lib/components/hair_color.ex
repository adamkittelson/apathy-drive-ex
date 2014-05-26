defmodule Components.HairColor do
  use GenEvent.Behaviour

  ### Public API
  def value(entity) do
    :gen_event.call(entity, Components.HairColor, :value)
  end

  def value(entity, new_value) do
    Entity.notify(entity, {:set_hair_color, new_value})
  end

  def serialize(entity) do
    %{"HairColor" => value(entity)}
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, value) do
    {:ok, value, value}
  end

  def handle_event({:set_hair_color, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
