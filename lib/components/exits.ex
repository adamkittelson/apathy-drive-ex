defmodule Components.Exits do
  use GenEvent.Behaviour

  ### Public API
  def value(entity) do
    :gen_event.call(entity, Components.Exits, :value)
  end

  def value(entity, new_value) do
    Entity.notify(entity, {:set_exits, new_value})
  end

  def get_exits(entity) do
    :gen_event.call(entity, Components.Exits, :get_exits)
  end

  def set_exits(entity, exits) do
    value(entity, Enum.map(exits, &(&1 |> Components.ID.value)))
  end

  def serialize(entity) do
    %{"Exits" => value(entity)}
  end

  ### GenEvent API
  def init(exit_ids) do
    {:ok, exit_ids}
  end

  def handle_call(:value, exit_ids) do
    {:ok, exit_ids, exit_ids}
  end

  def handle_call(:get_exits, exit_ids) do
    {:ok, Enum.map(exit_ids, &(Components.find_by(Components.ID, &1))), exit_ids}
  end

  def handle_event({:set_exits, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end