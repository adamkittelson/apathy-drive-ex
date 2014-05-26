defmodule Components.Number do
  use GenEvent.Behaviour

  ### Public API
  def value(entity) do
    :gen_event.call(entity, Components.Number, :get_number)
  end

  def get_number(entity) do
    value(entity)
  end

  def value(entity, new_value) do
    Entity.notify(entity, {:set_number, new_value})
  end

  def serialize(entity) do
    %{"Number" => get_number(entity)}
  end

  ### GenEvent API
  def init(number) do
    {:ok, number}
  end

  def handle_call(:get_number, number) do
    {:ok, number, number}
  end

  def handle_event({:set_number, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
