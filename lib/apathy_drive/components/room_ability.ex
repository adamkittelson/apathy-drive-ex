defmodule Components.RoomAbility do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.RoomAbility, :value)
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_room_ability, new_value})
  end

  def serialize(entity) do
    %{"RoomAbility" => value(entity)}
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, value) do
    {:ok, value, value}
  end

  def handle_event(_, value) do
    {:ok, value}
  end
end
