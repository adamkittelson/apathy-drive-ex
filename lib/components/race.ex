defmodule Components.Race do
  use GenEvent.Behaviour

  ### Public API
  def value(entity) do
    :gen_event.call(entity, Components.Race, :value)
  end

  def serialize(entity) do
    {"Race", Components.Number.get_number(value(entity))}
  end

  ### GenEvent API
  def init(value) when is_number(value) do
    {:ok, Races.find_by_number(value)}
  end

  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, value) do
    {:ok, value, value}
  end

  def handle_cast({:set_value, value}, _value) do
    {:noreply, value }
  end
end
