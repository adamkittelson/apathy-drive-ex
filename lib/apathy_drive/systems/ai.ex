defmodule Systems.AI do

  def think(%Monster{} = monster) do
    if Monster.on_global_cooldown?(monster) do
      move(monster)
    else
      heal(monster) || bless(monster) || attack(monster) || move(monster)
    end
  end

  def heal(%Monster{hp: hp} = monster) do
    max_hp  = Monster.max_hp(monster)

    chance = trunc((max_hp - hp) / max_hp * 100)

    roll = :random.uniform(100)

    if chance > roll do
      ability = monster
                |> Monster.heal_abilities
                |> random_ability

      if ability do
        send(self, {:execute_ability, ability})
      end
    end
  end

  def bless(%Monster{} = monster) do
    ability = monster
              |> Monster.bless_abilities
              |> random_ability

    if ability do
      send(self, {:execute_ability, ability})
    end
  end

  def attack(monster) do
    if target = Monster.aggro_target(monster) do
      attack = monster
               |> Monster.attack_abilities
               |> random_ability

      send(self, {:execute_ability, attack, target})
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

  def move(monster) do
    if !Monster.on_ai_move_cooldown?(monster) && !Monster.aggro_target(monster) do

      room = Monster.find_room(monster)
      roll = :random.uniform(100)

      if room && (roll < trunc(monster.chance_to_follow / 5)) do
        direction = Room.random_direction(room)

        if direction do
          Monster.execute_command(self, direction, [])
        end
      end
    end
  end

end
