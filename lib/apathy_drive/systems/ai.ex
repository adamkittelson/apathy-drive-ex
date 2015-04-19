defmodule Systems.AI do

  def think(%Monster{} = monster) do
    heal(monster) || bless(monster) || attack(monster) || move(monster) || monster
  end

  def heal(%Monster{hp: hp, max_hp: max_hp, spirit: nil} = monster) do
    unless Monster.on_global_cooldown?(monster) do
      chance = trunc((max_hp - hp) / max_hp * 100)

      roll = :random.uniform(100)

      if chance > roll do
        ability = monster
                  |> Monster.heal_abilities
                  |> random_ability

        if ability do
          send(self, {:execute_ability, ability})
          monster
        end
      end
    end
  end
  def heal(%Monster{}), do: nil

  def bless(%Monster{spirit: nil} = monster) do
    unless Monster.on_global_cooldown?(monster) do
      ability = monster
                |> Monster.bless_abilities
                |> random_ability

      if ability do
        send(self, {:execute_ability, ability})
        monster
      end
    end
  end
  def bless(%Monster{}), do: nil

  def attack(%Monster{spirit: nil} = monster) do
    if target = Monster.aggro_target(monster) do

      attack = cond do
        !Monster.on_attack_cooldown?(monster) ->
          monster
           |> Monster.monster_attacks
           |> random_ability
        !Monster.on_global_cooldown?(monster) ->
           monster
           |> Monster.attack_abilities
           |> random_ability
        true ->
          nil
      end

      if attack do
        send(self, {:execute_ability, attack, [target]})
        monster
      end
    end
  end

  def attack(%Monster{} = monster) do
    if target = Monster.aggro_target(monster) do

      if !Monster.on_attack_cooldown?(monster) do
        attack = monster
                 |> Monster.monster_attacks
                 |> random_ability

        send(self, {:execute_ability, attack, [target]})
        monster
      end
    end
  end

  def random_ability(abilities) do
    case abilities do
      [ability] ->
        ability
      [] ->
        nil
      abilities ->
        abilities |> Enum.shuffle |> List.first
    end
  end

  def move(%Monster{spirit: nil} = monster) do
    if !Monster.on_ai_move_cooldown?(monster) && !Monster.aggro_target(monster) do

      room = Monster.find_room(monster)
      roll = :random.uniform(100)

      if room && (roll < trunc(monster.chance_to_follow / 5)) do
        direction = Room.random_direction(room)

        if direction do
          ApathyDrive.Command.execute(monster, direction, [])
        end
      end
    end
  end
  def move(%Monster{}), do: nil

end
