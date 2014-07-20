defmodule Components.Attacks do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Attacks, :value)
  end

  def reset_attacks(entity) do
    attacks = racial_attacks(entity) ++ item_attacks(entity) ++ monster_attacks(entity)
              |> List.flatten
              |> Enum.reduce(%{}, fn(attack, map) ->
                    put_in(map[(highest_key(map) || 0) + attack["weight"]], attack)
                 end)

    GenEvent.notify(entity, {:set_attacks, attacks})
  end

  def add_attack(entity, attack) do
    GenEvent.notify(entity, {:add_attack, attack})
  end

  def remove_attack(entity, attack) do
    GenEvent.notify(entity, {:remove_attack, attack})
  end

  def random(entity) do
    :random.seed(:os.timestamp)

    attacks = value(entity)

    roll = attacks
           |> highest_key
           |> :random.uniform

    key = attacks
          |> Map.keys
          |> Enum.sort
          |> Enum.find(&(&1 >= roll))

    attacks[key]
  end

  def serialize(_entity) do
    %{"Attacks" => %{}}
  end

  defp highest_key(attacks) do
    attacks
    |> Map.keys
    |> Enum.sort
    |> List.last
  end

  defp racial_attacks(entity) do
    if Entity.has_component?(entity, Components.Race) do
      entity
      |> Components.Race.value
      |> extract_attacks(entity)
    else
      []
    end
  end

  defp item_attacks(entity) do
    entity
    |> Systems.Limbs.equipped_items
    |> Enum.map(&extract_attacks(&1, entity))
    |> List.flatten
  end

  defp monster_attacks(entity) do
    extract_attacks(entity, entity)
  end

  def extract_attacks(entity, owner) do
    if Entity.has_component?(entity, Components.Module) do
      module = Components.Module.value(entity)
      if function_exported?(module, :attacks, 1) do
         module.attacks(owner)
       else
         []
       end
    else
      []
    end
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, attacks) do
    {:ok, attacks, attacks}
  end

  def handle_event({:set_attacks, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event({:add_attack, attack}, value) do
    {:ok, put_in(value[highest_key(value) + attack["weight"]], attack)}
  end

  def handle_event({:remove_character, character}, value) do
    {:ok, List.delete(value, character) }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end