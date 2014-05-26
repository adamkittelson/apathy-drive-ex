defmodule Components.WornOn do
  use GenEvent.Behaviour

  ### Public API
  def value(entity) do
    :gen_event.call(entity, Components.WornOn, :get_worn_on)
  end

  def value(entity, new_value) do
    ApathyDrive.Entity.notify(entity, {:set_worn_on, new_value})
  end

  def serialize(entity) do
    %{"WornOn" => value(entity)}
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:get_worn_on, value) do
    {:ok, value, value}
  end

  def handle_event({:set_value, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
