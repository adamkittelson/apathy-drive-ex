defmodule Components.HPRolls do
  use GenEvent.Behaviour

  ### Public API
  def value(entity) do
    :gen_event.call(entity, Components.HPRolls, :get_hp_rolls)
  end

  def value(entity, new_value) do
    ApathyDrive.Entity.notify(entity, {:set_hp_rolls, new_value})
  end

  def serialize(entity) do
    %{"HPRolls" => value(entity)}
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:get_hp_rolls, hp_rolls) do
    {:ok, hp_rolls, hp_rolls}
  end

  def handle_event({:set_hp_rolls, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
