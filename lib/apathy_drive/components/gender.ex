defmodule Components.Gender do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Gender, :value)
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_gender, new_value})
  end

  def serialize(entity) do
    %{"Gender" => value(entity)}
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, value) do
    {:ok, value, value}
  end

  def handle_event({:set_gender, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
