defmodule Components.Abilities do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Abilities, :value)
  end

  def heals(entity) do
    entity
    |> value
    |> useable(entity)
    |> Enum.filter(fn(ability) ->
         Components.Module.value(ability).properties(entity)
         |> Map.keys
         |> Enum.member?(:healing)
       end)
  end

  def useable(abilities, entity) do
    if casting?(entity) do
      []
    else
      mana = Components.Mana.value(entity)
      Enum.filter(abilities, fn(ability) ->
        Components.Module.value(ability).mana_cost <= mana
      end)
    end
  end

  def casting?(monster) do
    monster
    |> Components.Effects.value
    |> Map.values
    |> Enum.any?(fn(effect) ->
         effect[:stack_key] == :cast_timer
       end)
  end

  def reset_abilities(entity) do
    abilities = Abilities.from_skills(entity) ++ from_module(entity)

    GenEvent.notify(entity, {:set_abilities, abilities})
  end

  def from_module(entity) do
    if Entity.has_component?(entity, Components.Module) do
      Components.Module.value(entity).abilities
      |> Enum.map(fn(ability_name) ->
           Abilities.find ability_name
         end)
      |> Enum.reject(fn(ability) -> ability == nil end)
      |> Enum.map(fn(module) ->
           Abilities.find_by_module(module)
         end)
    else
      []
    end
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