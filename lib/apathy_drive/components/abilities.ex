defmodule Components.Abilities do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Abilities, :value)
  end

  def reset_abilities(entity) do
    abilities = Abilities.from_skills(entity)

    GenEvent.notify(entity, {:set_abilities, abilities})
  end

  def names(entity) do
    entity
    |> value
    |> Enum.map(&(Components.Module.value(&1).name))
  end

  def serialize(_entity) do
    %{"Abilities" => []}
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, attacks) do
    {:ok, attacks, attacks}
  end

  def handle_event({:set_abilities, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end