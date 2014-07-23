defmodule Components.Hunting do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Hunting, :value)
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_hunting, new_value})
  end

  def add(entity, target) do
    GenEvent.notify(entity, {:add_hunting, target})
  end

  def remove(entity, target) do
    GenEvent.notify(entity, {:remove_hunting, target})
  end

  def serialize(_entity) do
    %{"Hunting" => []}
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, hunting) do
    {:ok, hunting, hunting}
  end

  def handle_event({:set_hunting, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event({:add_hunting, target}, value) do
    {:ok, [target | value] |> Enum.uniq }
  end

  def handle_event({:remove_hunting, target}, value) do
    {:ok, List.delete(value, target) }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end