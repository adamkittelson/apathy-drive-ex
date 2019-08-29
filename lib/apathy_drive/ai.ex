defmodule ApathyDrive.AI do
  alias ApathyDrive.{Ability, Character, Mobile, Monster, Party, Room}

  def think(%Room{} = room, ref) do
    Room.update_mobile(room, ref, fn mobile ->
      if mobile.casting do
        flee(mobile, room) || mobile
      else
        drain(mobile, room) || heal(mobile, room) || flee(mobile, room) || bless(mobile, room) ||
          curse(mobile, room) || attack(mobile, room) || auto_attack(mobile, room) ||
          move(mobile, room) || mobile
      end
    end)
  end

  def move(%{} = mobile, %Room{} = room, force \\ false) do
    if should_move?(mobile, room) or force do
      exits =
        case room.exits do
          nil ->
            []

          _exits ->
            exits_in_area(room, mobile)
        end

      if Enum.any?(exits) do
        new_exits =
          exits
          |> Enum.reject(&(&1["destination"] == mobile.last_room_id))

        room_exit =
          cond do
            is_nil(mobile.last_room_id) ->
              Enum.random(exits)

            Enum.any?(new_exits) ->
              Enum.random(new_exits)

            :else ->
              Enum.find(exits, &(&1["destination"] == mobile.last_room_id))
          end

        case ApathyDrive.Commands.Move.execute(room, mobile, room_exit, false) do
          %Room{} = room ->
            room

          {:error, :too_tired, room} ->
            room
        end
      end
    end
  end

  def flee(%{} = mobile, %Room{} = room) do
    if should_flee?(mobile, room) do
      exits =
        case room.exits do
          nil ->
            []

          _exits ->
            exits_in_area(room, mobile)
        end

      if Enum.any?(exits) do
        case ApathyDrive.Commands.Move.execute(room, mobile, Enum.random(exits), false) do
          %Room{} = room ->
            room

          {:error, :too_tired, room} ->
            room
        end
      end
    end
  end

  def drain(%{auto_heal: true} = mobile, %Room{} = room) do
    target = room.mobiles[Mobile.auto_attack_target(mobile, room)]
    chance = trunc(:math.pow(20, 2 - mobile.hp) - 20)

    roll = :rand.uniform(100)

    if target && chance > roll do
      ability =
        mobile
        |> Ability.drain_abilities(target)
        |> random_ability(mobile)

      if ability do
        Ability.execute(room, mobile.ref, ability, [target.ref])
      end
    end
  end

  def drain(%{} = _mobile, %Room{}), do: nil

  def heal(%{auto_heal: true} = mobile, %Room{} = room) do
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

  def heal(%{} = _mobile, %Room{}), do: nil

  def bless(%{auto_bless: true, mana: mana} = mobile, %Room{} = room) when mana > 0.5 do
    member_to_bless =
      room
      |> Party.members(mobile)
      |> Enum.random()

    ability =
      mobile
      |> Ability.bless_abilities(member_to_bless)
      |> reject_self_only_if_not_targetting_self(mobile, member_to_bless)
      |> reject_light_spells_if_it_isnt_dark(room)
      |> reject_target_has_ability_passively(member_to_bless)
      |> random_ability(mobile)

    if ability do
      Ability.execute(room, mobile.ref, ability, [member_to_bless.ref])
    end
  end

  def bless(%{} = _mobile, %Room{}), do: nil

  def curse(%{auto_curse: true} = mobile, %Room{} = room) do
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

  def attack(%{auto_nuke: true} = mobile, %Room{} = room) do
    target = room.mobiles[Mobile.auto_attack_target(mobile, room)]

    energy_pct = mobile.energy / mobile.max_energy * 100

    if target && energy_pct >= 40 do
      potential_targets =
        Ability.get_targets(room, mobile.ref, %Ability{targets: "full attack area"}, "")

      attack_abilities = Ability.attack_abilities(mobile, target)

      attack_abilities =
        if mobile.__struct__ == Monster do
          attack_abilities
          |> Enum.filter(&is_nil(&1.chance))
        else
          attack_abilities
        end

      {ability, targets} =
        if length(potential_targets) > 1 and
             Enum.any?(attack_abilities, &(&1.targets == "full attack area")) do
          ability =
            attack_abilities
            |> Enum.filter(&(&1.targets == "full attack area"))
            |> random_ability(mobile)

          {ability, ""}
        else
          ability =
            attack_abilities
            |> Enum.reject(&(&1.targets == "full attack area"))
            |> random_ability(mobile)

          {ability, [target.ref]}
        end

      if ability do
        Ability.execute(room, mobile.ref, ability, targets)
      end
    end
  end

  def attack(%{} = _mobile, %Room{}), do: nil

  def auto_attack(mobile, room) do
    if target_ref = Mobile.auto_attack_target(mobile, room) do
      if mobile.energy == mobile.max_energy && !mobile.casting do
        {attacks, _energy} =
          Enum.reduce(1..5, {[], mobile.energy}, fn _n, {attacks, energy} ->
            attack = Mobile.attack_ability(mobile)

            if attack && attack.energy <= energy do
              {[attack | attacks], energy - attack.energy}
            else
              {attacks, energy}
            end
          end)

        if Enum.any?(attacks) do
          Enum.reduce(attacks, room, fn attack, room ->
            Ability.execute(room, mobile.ref, attack, [target_ref])
          end)
        end
      end
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

        _mobile ->
          nil
      end
    end
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

  def exits_in_area(%Room{exits: exits} = room, %Character{} = mobile) do
    Enum.filter(exits, fn %{"direction" => _direction} = room_exit ->
      room_exit["area"] == room.area.id && passable?(room, room_exit, mobile)
    end)
  end

  def exits_in_area(%Room{exits: exits} = room, mobile) do
    Enum.filter(exits, fn %{"direction" => _direction} = room_exit ->
      room_exit["zone"] == room.zone_controller_id && passable?(room, room_exit, mobile)
    end)
  end

  defp passable?(room, %{"kind" => kind} = room_exit, _mobile) when kind in ["Door", "Gate"],
    do: ApathyDrive.Doors.open?(room, room_exit)

  defp passable?(_room, %{"kind" => kind}, %Character{})
       when kind in ["Block Guard", "Normal", "Action", "Trap", "Cast"],
       do: true

  defp passable?(_room, %{"kind" => kind}, _mobile)
       when kind in ["Normal", "Action", "Trap", "Cast"],
       do: true

  defp passable?(_room, _room_exit, _mobile), do: false

  defp should_move?(%ApathyDrive.Companion{}, _room), do: false

  defp should_move?(%ApathyDrive.Character{auto_roam: true} = character, room) do
    character.hp > 0.8 and character.mana > 0.5 and
      is_nil(Mobile.auto_attack_target(character, room))
  end

  defp should_move?(%{movement: "stationary"}, _room), do: false

  defp should_move?(%{auto_roam: true} = mobile, room) do
    is_nil(Mobile.auto_attack_target(mobile, room)) and :rand.uniform(100) > 99
  end

  defp should_move?(%{} = _mobile, _room), do: false

  defp should_flee?(%ApathyDrive.Companion{}, _room), do: false

  defp should_flee?(%Monster{movement: "stationary"}, _room), do: false

  defp should_flee?(%Monster{} = monster, room) do
    is_nil(Mobile.auto_attack_target(monster, room)) and :rand.uniform(100) > 99
  end

  defp should_flee?(%{auto_flee: true} = mobile, room) do
    hp_low? = mobile.hp < 0.20

    enemies_present? =
      room.mobiles
      |> Map.values()
      |> Enum.any?(fn
        %Monster{hostile: true} ->
          true

        _ ->
          false
      end)

    hp_low? and enemies_present?
  end

  defp should_flee?(%{} = _mobile, _room), do: false

  defp reject_self_only_if_not_targetting_self(abilities, mobile, member_to_bless) do
    if mobile.ref !== member_to_bless.ref do
      Enum.reject(abilities, &(&1.targets == "self"))
    else
      abilities
    end
  end

  defp reject_target_has_ability_passively(abilities, member_to_bless) do
    abilities
    |> Enum.reject(&Ability.has_passive_ability?(member_to_bless, &1.id))
  end

  defp reject_light_spells_if_it_isnt_dark(abilities, room) do
    # don't cast positive light spells if the room isn't dark
    # but treat darkness spells like regular blessings
    if Room.light(room) >= 0 do
      Enum.reject(abilities, &(!is_nil(&1.traits["Light"]) and &1.traits["Light"] > 0))
    else
      abilities
    end
  end
end
