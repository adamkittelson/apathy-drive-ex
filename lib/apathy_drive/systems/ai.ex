defmodule Systems.AI do

  def think(%Monster{} = monster) do
    if Monster.on_global_cooldown?(monster) do
      #move(monster)
    else
      heal(monster) || bless(monster) || attack(monster)# || move(monster)
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
    if !Components.Combat.in_combat?(monster) do
      :random.seed(:os.timestamp)

      room = Parent.of(monster)
      roll = :random.uniform(100)

      if room && (roll < trunc(Components.Module.value(monster).chance_to_follow / 5)) do

        if !Entity.has_component?(room, Components.PermanentNPC) or
          !String.contains?(Components.Name.value(monster), Components.PermanentNPC.value(room)) do

            direction = room
                        |> Components.Exits.value
                        |> Enum.map(&(Map.get(&1, "direction")))
                        |> Enum.shuffle
                        |> List.first

            if direction do
              Systems.Room.move(Possession.possessor(monster), monster, direction)
            end
        end
      end
    end
  end

end
