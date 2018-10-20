defmodule ApathyDrive.AI do
  alias ApathyDrive.{Ability, Mobile, Party, Room}

  def think(%Room{} = room, ref) do
    mobile = room.mobiles[ref]

    if mobile.casting do
      mobile
    else
      heal(mobile, room) || bless(mobile, room) || curse(mobile, room) || attack(mobile, room) ||
        auto_attack(mobile, room)
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

  def bless(%{mana: mana} = mobile, %Room{} = room) when mana > 0.5 do
    member_to_bless =
      room
      |> Party.members(mobile)
      |> Enum.random()

    ability =
      mobile
      |> Ability.bless_abilities(member_to_bless)
      |> random_ability(mobile)

    if ability do
      Ability.execute(room, mobile.ref, ability, [member_to_bless.ref])
    end
  end

  def bless(%{} = _mobile, %Room{}), do: nil

  def curse(%{mana: mana} = mobile, %Room{} = room) when mana > 0.9 do
    target = room.mobiles[Mobile.auto_attack_target(mobile, room)]

    if target && :rand.uniform(100) > 95 do
      ability =
        mobile
        |> Ability.curse_abilities(target)
        |> random_ability(mobile)

      if ability do
        Ability.execute(room, mobile.ref, ability, [target.ref])
      end
    end
  end

  def curse(%{} = _mobile, %Room{}), do: nil

  def attack(%{mana: mana} = mobile, %Room{} = room) when mana > 0.75 do
    target = room.mobiles[Mobile.auto_attack_target(mobile, room)]

    if target do
      ability =
        mobile
        |> Ability.attack_abilities(target)
        |> random_ability(mobile)

      if ability do
        Ability.execute(room, mobile.ref, ability, [target.ref])
      end
    end
  end

  def attack(%{} = _mobile, %Room{}), do: nil

  def auto_attack(mobile, room) do
    Room.update_mobile(room, mobile.ref, fn %{} = mobile ->
      attack = Mobile.attack_ability(mobile)

      if attack && mobile.energy >= attack.energy && !mobile.casting do
        if target_ref = Mobile.auto_attack_target(mobile, room) do
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
