defmodule ApathyDrive.AI do
  alias ApathyDrive.{Ability, Mobile, Party, Room}

  def think(%Room{} = room, ref) do
    mobile = room.mobiles[ref]

    if mobile.casting do
      mobile
    else
      # || bless(room, monster_ref) || curse(room, monster_ref) || attack(room, monster_ref) || room
      heal(mobile, room) || auto_attack(mobile, room)
    end
  end

  def heal(%{} = mobile, %Room{} = room) do
    injured_party_member =
      room
      |> Party.members(mobile)
      |> Enum.sort_by(& &1.hp)
      |> List.first()

    chance = trunc(:math.pow(20, 2 - injured_party_member.hp) - 20)

    roll = :rand.uniform(100)

    if chance > roll do
      ability =
        mobile
        |> Ability.heal_abilities()
        |> random_ability(mobile)

      if ability do
        Ability.execute(room, mobile.ref, ability, [injured_party_member.ref])
      end
    end
  end

  def auto_attack(mobile, room) do
    Room.update_mobile(room, mobile.ref, fn %{} = mobile ->
      attack = Mobile.attack_ability(mobile)

      if attack && mobile.energy >= attack.energy && !mobile.casting do
        if target_ref = Mobile.auto_attack_target(mobile, room, attack) do
          Ability.execute(room, mobile.ref, attack, [target_ref])
        else
          case mobile do
            %{attack_target: target} = mobile when not is_nil(target) ->
              Mobile.send_scroll(
                mobile,
                "<p><span class='dark-yellow'>*Combat Off*</span></p>"
              )

              mobile =
                mobile
                |> Map.put(:attack_target, nil)

              room = put_in(room.mobiles[mobile.ref], mobile)

              Room.update_hp_bar(room, mobile.ref)
              Room.update_mana_bar(room, mobile.ref)

              room

            mobile ->
              mobile
          end
        end
      else
        mobile
      end
    end)
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

  def random_ability([ability], mobile) do
    unless Ability.on_cooldown?(mobile, ability) do
      ability
    end
  end

  def random_ability([], _mobile), do: nil

  def random_ability(abilities, mobile) do
    abilities
    |> Enum.reject(&Ability.on_cooldown?(mobile, &1))
    |> Enum.random()
  end
end
