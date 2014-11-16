defmodule Components.Exits do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Exits, :value)
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_exits, new_value})
  end

  def direction(entity, direction) do
    GenEvent.call(entity, Components.Exits, {:get_direction, direction})
  end

  def open_door(entity, direction) do
    GenEvent.call(entity, Components.Exits, {:open_door, direction})
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

  def handle_call({:get_direction, direction}, exit_ids) do
    {:ok, exit_ids |> Enum.find(&(&1["direction"] == direction)), exit_ids}
  end

  def handle_call({:open_door, direction}, exits) do
    exits = exits
            |> Enum.map(fn(room_exit) ->
                 if room_exit["direction"] == direction do
                   Map.put(room_exit, :open, true)
                 else
                   room_exit
                 end
               end)
    {:ok, exits, exits}
  end

  def handle_event({:set_exits, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end