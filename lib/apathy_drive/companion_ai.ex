defmodule ApathyDrive.CompanionAI do
  alias ApathyDrive.{Companion, Party, Room, Spell}

  def think(companion_ref, %Room{} = room) do
    companion = room.mobiles[companion_ref]
    heal(companion, room) || companion #|| bless(room, monster_ref) || curse(room, monster_ref) || attack(room, monster_ref) || room
  end

  def heal(%Companion{} = companion, %Room{} = room) do
    injured_party_member =
      room
      |> Party.members(companion)
      |> Enum.sort_by(& &1.hp)
      |> List.first

    chance = trunc(:math.pow(20, 2 - injured_party_member.hp) - 20)

    roll = :rand.uniform(100)

    if chance > roll do
      spell =
        companion
        |> Spell.heal_spells
        |> random_spell(companion)

      if spell do
        Spell.execute(room, companion.ref, spell, [injured_party_member.ref])
      end
    end
  end

  # def bless(%Room{} = room, monster_ref) do
  #   case Room.get_monster(room, monster_ref) do
  #     %{spirit: nil} = monster ->
  #       unless Ability.on_global_cooldown?(monster) do
  #         ability = monster
  #                   |> Ability.bless_abilities
  #                   |> random_ability(monster)
  #
  #         if ability do
  #           Ability.execute(room, monster_ref, ability, [monster_ref])
  #         end
  #       end
  #     _ ->
  #       nil
  #   end
  # end
  #
  # def curse(%Room{} = room, monster_ref) do
  #   case Room.get_monster(room, monster_ref) do
  #     %{spirit: nil} = monster ->
  #       if :rand.uniform(100) > 95 do
  #         if target = Monster.aggro_target(room, monster) do
  #           curse =
  #             if !Ability.on_global_cooldown?(monster) do
  #
  #               monster
  #               |> Ability.curse_abilities
  #               |> random_ability(monster)
  #             end
  #
  #           if curse do
  #             Ability.execute(room, monster_ref, curse, [target])
  #           end
  #         end
  #       end
  #     _ ->
  #       nil
  #   end
  # end
  #
  # def attack(%Room{} = room, monster_ref) do
  #   case Room.get_monster(room, monster_ref) do
  #     %{spirit: nil} = monster ->
  #       if target = Monster.aggro_target(room, monster) do
  #         attack = cond do
  #           !Ability.on_global_cooldown?(monster) ->
  #              monster
  #              |> Ability.attack_abilities
  #              |> random_ability(monster)
  #           true ->
  #             nil
  #         end
  #
  #         if attack do
  #           room
  #           |> Ability.execute(monster_ref, attack, [target])
  #           |> Room.update_monster(monster_ref, fn(mob) ->
  #                mob
  #                |> Monster.set_attack_target(target)
  #                |> Monster.initiate_combat
  #              end)
  #         else
  #           Room.update_monster(room, monster_ref, fn(mob) ->
  #             mob
  #             |> Monster.set_attack_target(target)
  #             |> Monster.initiate_combat
  #           end)
  #         end
  #       end
  #     %{spirit: _} = monster ->
  #       if target = Monster.aggro_target(room, monster) do
  #
  #         Room.update_monster(room, monster_ref, fn(mob) ->
  #           mob
  #           |> Monster.set_attack_target(target)
  #           |> Monster.initiate_combat
  #         end)
  #       end
  #   end
  # end

  def random_spell([spell], companion) do
    unless Spell.on_cooldown?(companion, spell) do
      spell
    end
  end
  def random_spell([], companion), do: nil
  def random_spell(spells, companion) do
    spells
    |> Enum.reject(&(Spell.on_cooldown?(companion, &1)))
    |> Enum.random
  end

end
