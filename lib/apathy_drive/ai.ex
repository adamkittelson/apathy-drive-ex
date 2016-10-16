defmodule ApathyDrive.AI do
  alias ApathyDrive.{Room, Monster, Ability, TimerManager}

  def think(%Room{} = room, monster_ref) do
    room =
      Room.update_monster(room, monster_ref, fn monster ->
        room
        |> calm_down(monster)
        |> TimerManager.send_after({:monster_ai, 5_000, {:think, monster_ref}})
      end)

    if casting?(room, monster_ref) do
      room
    else
      heal(room, monster_ref) || bless(room, monster_ref) || curse(room, monster_ref) || attack(room, monster_ref) || room
    end
  end

  def calm_down(%Room{mobiles: monsters}, %Monster{hate: hate} = monster) do
    monsters_in_room =
      monsters
      |> Map.keys
      |> Enum.into(HashSet.new)

    hate =
      hate
      |> Map.keys
      |> Enum.into(HashSet.new)
      |> HashSet.difference(monsters_in_room)
      |> Enum.reduce(hate, fn(enemy, new_hate) ->
           current = Map.get(new_hate, enemy)
           if current > 5 do
             Map.put(new_hate, enemy, current - 5)
           else
             Map.delete(new_hate, enemy)
           end
         end)

    Map.put(monster, :hate, hate)
  end

  def heal(%Room{} = room, monster_ref) do
    case Room.get_monster(room, monster_ref) do
      %Monster{hp: hp, max_hp: max_hp, spirit: nil} = monster ->
        unless Ability.on_global_cooldown?(monster) do
          chance = trunc((max_hp - hp) / max_hp * 100)

          roll = :rand.uniform(100)

          if chance > roll do
            ability =
              monster
              |> Ability.heal_abilities
              |> random_ability(monster)

            if ability do
              Ability.execute(room, monster_ref, ability, [monster_ref])
            end
          end
        end
      _ ->
        nil
    end
  end

  def bless(%Room{} = room, monster_ref) do
    case Room.get_monster(room, monster_ref) do
      %Monster{spirit: nil} = monster ->
        unless Ability.on_global_cooldown?(monster) do
          ability = monster
                    |> Ability.bless_abilities
                    |> random_ability(monster)

          if ability do
            Ability.execute(room, monster_ref, ability, [monster_ref])
          end
        end
      _ ->
        nil
    end
  end

  def curse(%Room{} = room, monster_ref) do
    case Room.get_monster(room, monster_ref) do
      %Monster{spirit: nil} = monster ->
        if :rand.uniform(100) > 95 do
          if target = Monster.aggro_target(room, monster) do
            curse =
              if !Ability.on_global_cooldown?(monster) do

                monster
                |> Ability.curse_abilities
                |> random_ability(monster)
              end

            if curse do
              Ability.execute(room, monster_ref, curse, [target])
            end
          end
        end
      _ ->
        nil
    end
  end

  def attack(%Room{} = room, monster_ref) do
    case Room.get_monster(room, monster_ref) do
      %Monster{spirit: nil} = monster ->
        if target = Monster.aggro_target(room, monster) do
          attack = cond do
            !Ability.on_global_cooldown?(monster) ->
               monster
               |> Ability.attack_abilities
               |> random_ability(monster)
            true ->
              nil
          end

          if attack do
            room
            |> Ability.execute(monster_ref, attack, [target])
            |> Room.update_monster(monster_ref, fn(mob) ->
                 mob
                 |> Monster.set_attack_target(target)
                 |> Monster.initiate_combat
               end)
          else
            Room.update_monster(room, monster_ref, fn(mob) ->
              mob
              |> Monster.set_attack_target(target)
              |> Monster.initiate_combat
            end)
          end
        end
      %{spirit: _} = monster ->
        if target = Monster.aggro_target(room, monster) do

          Room.update_monster(room, monster_ref, fn(mob) ->
            mob
            |> Monster.set_attack_target(target)
            |> Monster.initiate_combat
          end)
        end
    end
  end

  def random_ability(abilities, monster) do
    case abilities do
      [ability] ->
        unless Ability.on_cooldown?(monster, ability) do
          ability
        end
      [] ->
        nil
      abilities ->
        abilities
        |> Enum.reject(&(Ability.on_cooldown?(monster, &1)))
        |> Enum.random
    end
  end

  defp casting?(%Room{} = room, monster_ref) do
    case Room.get_monster(room, monster_ref) do
      %Monster{timers: timers} ->
        !!Map.get(timers, :cast_timer)
      nil -> true
    end
  end

end
