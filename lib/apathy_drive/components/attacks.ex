defmodule Components.Attacks do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Attacks, :value)
  end

  def reset_attacks(entity) do
    attacks = item_attacks(entity) ++ monster_attacks(entity)
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

  def item_attacks(entity) do
    entity
    |> Systems.Limbs.equipped_items
    |> Enum.map(&extract_attacks(&1, entity))
    |> List.flatten
  end

  def monster_attacks(entity) do
    damage_increases = entity
                       |> Components.Effects.value
                       |> Map.values
                       |> damage_increases

    extract_attacks(entity, entity)
    |> Enum.map(fn(attack) ->
         damage_increases
         |> Enum.reduce(attack, fn(damage_increase, attack) ->
              update_in(attack, ["damage", damage_increase[:table]], &((&1 || 0) + damage_increase[:amount]))
            end)
       end)
  end

  def damage_increases(effects) do
    effects
    |> Enum.filter(fn(effect) ->
         Map.has_key?(effect, :damage_increase)
       end)
    |> Enum.map(fn(effect) ->
         Map.get(effect, :damage_increase)
       end)
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