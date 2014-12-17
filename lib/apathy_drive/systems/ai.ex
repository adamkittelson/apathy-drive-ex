defmodule Systems.AI do
  use Systems.Reload

  def observable_monsters do
    Characters.online
    |> Enum.map(&Parent.of/1)
    |> Enum.uniq
    |> adjacent_rooms
    |> adjacent_rooms
    |> adjacent_rooms
    |> Enum.map(&Components.Monsters.get_monsters/1)
    |> List.flatten
    |> Enum.uniq
  end

  def adjacent_rooms(rooms_with_spirits) do
    Enum.reduce(rooms_with_spirits, [], fn(room, rooms) ->
      adjacent = room
                 |> Components.Exits.value
                 |> Enum.map(&(Map.get(&1, "destination")))
                 |> Enum.map(&Rooms.find_by_id/1)

      [room | adjacent ++ rooms]
    end)
    |> Enum.uniq
  end

  def think do
    observable_monsters
    |> Enum.each(fn(monster) ->
         think(monster)
       end)
    :timer.sleep 5000
  end

  def think(monster) do
    if Process.alive?(monster) do
      use_ability?(monster)
    end
  end

  def use_ability?(monster) do
    heal(monster) || bless(monster) || attack(monster)
  end

  def heal(monster) do
    max_hp  = Systems.HP.max(monster)
    current = Components.HP.value(monster)

    chance = trunc((max_hp - current) / max_hp * 100)
    :random.seed(:os.timestamp)

    roll = :random.uniform(100)

    if chance > roll do
      ability = monster
                |> Components.Abilities.heals
                |> random_ability
      if ability do
        ability.execute(monster)
      end
    end
  end

  def bless(monster) do
    ability = monster
              |> Components.Abilities.blessings
              |> random_ability

    if ability do
      ability.execute(monster)
    end
  end

  def attack(monster) do
    if Components.Combat.in_combat?(monster) do
      :random.seed(:os.timestamp)

      roll = :random.uniform(100)

      if roll > 50 do
        ability = monster
                  |> Components.Abilities.attacks
                  |> random_ability

        if ability do
          target = Systems.Combat.targets(monster)
                   |> Enum.shuffle
                   |> List.first

          if target do
            ability.execute(monster, Components.Name.value(target))
          end
        end
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
        :random.seed(:os.timestamp)
        abilities |> Enum.shuffle |> List.first
    end
  end

end
