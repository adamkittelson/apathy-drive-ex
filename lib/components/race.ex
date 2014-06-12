defmodule Components.Race do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Race, :value)
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_race, new_value})
  end

  def serialize(entity) do
    %{"Race" => value(entity) |> Components.Name.value}
  end

  ### GenEvent API
  def init(value) when is_binary(value) do
    race = Races.find_by_name(value)
    {:ok, race}
  end

  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, value) do
    {:ok, value, value}
  end

  def handle_event({:set_race, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
