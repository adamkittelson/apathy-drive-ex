defmodule ApathyDrive.AI do
  alias ApathyDrive.{Ability, Character, Mobile, Monster, Party, Room}

  def think(%Room{} = room, ref) do
    Room.update_mobile(room, ref, fn room, mobile ->
      if mobile.casting do
        flee(mobile, room) || mobile
      else
        drain(mobile, room) || heal(mobile, room) || flee(mobile, room) || bless(mobile, room) ||
          backstab(mobile, room) || curse(mobile, room) || attack(mobile, room) ||
          auto_attack(mobile, room) || sneak(mobile, room) ||
          move(mobile, room) || mobile
      end
    end)
  end

  def move(%{} = mobile, %Room{} = room, force \\ false) do
    case should_move?(mobile, room) || force do
      :hp ->
        if Map.get(mobile, :auto_rest) && !Map.get(mobile, :resting) do
          ApathyDrive.Commands.Rest.execute(room, mobile, []).mobiles[mobile.ref]
        end

      :mana ->
        if Map.get(mobile, :auto_rest) && !Map.get(mobile, :resting) do
          ApathyDrive.Commands.Rest.execute(room, mobile, []).mobiles[mobile.ref]
        end

      true ->
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

      _other ->
        nil
    end
  end

  def sneak(%Character{} = character, %Room{} = room) do
    if character.auto_sneak && !character.sneaking &&
         !Mobile.auto_attack_target(character, room) && !character.resting do
      ApathyDrive.Commands.Sneak.execute(room, character, [])
    end
  end

  def sneak(_mobile, %Room{}), do: nil

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

  def drain(%{} = mobile, %Room{} = room) do
    target = room.mobiles[Mobile.auto_attack_target(mobile, room)]
    chance = trunc(:math.pow(20, 2 - mobile.hp) - 20)

    roll = :rand.uniform(100)

    if target && chance > roll do
      ability =
        mobile
        |> Ability.drain_abilities(target)
        |> Enum.reject(&(!&1.auto))
        |> random_ability(mobile)

      if ability do
        Ability.execute(room, mobile.ref, ability, [target.ref])
      end
    end
  end

  def pets_and_party(room, %Character{} = mobile) do
    party = Party.members(room, mobile)

    pets =
      party
      |> Enum.map(&pets(room, room.mobiles[&1.ref]))
      |> List.flatten()

    [party, pets]
    |> List.flatten()
    |> Enum.reject(&is_nil/1)
    |> Enum.uniq()
  end

  def pets_and_party(room, %Monster{} = mobile) do
    owner = owner(room, mobile)

    if owner do
      party = Party.members(room, owner)

      pets =
        party
        |> Enum.map(&pets(room, room.mobiles[&1.ref]))
        |> List.flatten()

      [party, pets]
      |> List.flatten()
      |> Enum.reject(&is_nil/1)
      |> Enum.uniq()
    else
      Party.members(room, mobile)
    end
  end

  def owner(room, mobile) do
    room.mobiles
    |> Map.values()
    |> Enum.find(fn mob ->
      if owner_id = Map.get(mobile, :owner_id) do
        owner_id == Map.get(mob, :room_monster_id) or owner_id == Map.get(mob, :id)
      end
    end)
  end

  def pets(room, mobile) do
    room.mobiles
    |> Map.values()
    |> Enum.filter(fn mob ->
      if owner_id = Map.get(mob, :owner_id) do
        owner_id == Map.get(mobile, :room_monster_id) or
          owner_id == mobile.id
      end
    end)
  end

  def heal(%{} = mobile, %Room{} = room) do
    injured_party_members = pets_and_party(room, mobile)

    injured_party_member =
      if Map.get(mobile, :auto_pet_casting) == false do
        injured_party_members
        |> Enum.reject(&(&1.__struct__ == Monster))
      else
        injured_party_members
      end
      |> Enum.sort_by(&injury_level/1)
      |> List.first()

    chance = trunc(:math.pow(15, 2 - injury_level(injured_party_member)) - 15)

    roll = :rand.uniform(100)

    if chance > roll do
      ability =
        mobile
        |> Ability.heal_abilities()
        |> Enum.reject(&(&1.targets == "self"))
        |> Enum.reject(&(!&1.auto))
        |> random_ability(mobile)

      if ability do
        Ability.execute(room, mobile.ref, ability, [injured_party_member.ref])
      end
    end
  end

  # average of health of lowest non severed limb and overall hp
  def injury_level(%{} = mobile) do
    lowest_limb =
      mobile.limbs
      |> Map.values()
      |> Enum.map(& &1.health)
      |> Enum.sort()
      |> Enum.reject(&(&1 <= 0.0))
      |> List.first()

    (lowest_limb + mobile.hp) / 2
  end

  def bless(%{mana: mana} = mobile, %Room{} = room) when mana > 0.5 do
    members_to_bless = pets_and_party(room, mobile)

    member_to_bless =
      if Map.get(mobile, :auto_pet_casting) == false do
        members_to_bless
        |> Enum.reject(&(&1.__struct__ == Monster))
      else
        members_to_bless
      end
      |> Enum.random()

    ability =
      mobile
      |> Ability.bless_abilities(member_to_bless)
      |> Enum.reject(&(!&1.auto))
      |> reject_self_only_if_not_targetting_self(mobile, member_to_bless)
      |> reject_item_if_monster(mobile)
      |> reject_elemental_if_no_lore(mobile)
      |> reject_light_spells_if_it_isnt_dark(room)
      |> reject_target_has_ability_passively(member_to_bless)
      |> random_ability(mobile)

    case ability do
      nil ->
        nil

      %Ability{targets: "weapon"} = ability ->
        if weapon = Character.weapon(mobile) do
          unless Systems.Effect.max_stacks?(weapon, ability) do
            Ability.execute(room, mobile.ref, ability, weapon)
          end
        end

      ability ->
        Ability.execute(room, mobile.ref, ability, [member_to_bless.ref])
    end
  end

  def bless(%{} = _mobile, %Room{}), do: nil

  def curse(%{} = mobile, %Room{} = room) do
    target = room.mobiles[Mobile.auto_attack_target(mobile, room)]

    if target && :rand.uniform(100) > 95 do
      ability =
        mobile
        |> Ability.curse_abilities(target)
        |> Enum.reject(&(!&1.auto))
        |> random_ability(mobile)

      if ability do
        Ability.execute(room, mobile.ref, ability, [target.ref])
      end
    end
  end

  def backstab(%Character{} = mobile, %Room{} = room) do
    target = room.mobiles[Mobile.auto_attack_target(mobile, room)]

    if mobile.sneaking && target do
      ApathyDrive.Commands.Backstab.execute(room, mobile, [target.name])
    end
  end

  def backstab(_mobile, %Room{}), do: nil

  def attack(%{} = mobile, %Room{} = room) do
    target = room.mobiles[Mobile.auto_attack_target(mobile, room)]

    energy_pct = mobile.energy / mobile.max_energy * 100

    if target && energy_pct >= 40 do
      potential_targets =
        Ability.get_targets(room, mobile.ref, %Ability{targets: "full attack area"}, "")

      attack_abilities =
        mobile
        |> Ability.attack_abilities(target)
        |> Enum.reject(&(!&1.auto))
        |> reject_elemental_if_no_lore(mobile)

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

  def auto_attack(mobile, room, target_ref \\ nil) do
    if target_ref = target_ref || Mobile.auto_attack_target(mobile, room) do
      attack = Mobile.attack_ability(mobile)

      if attack do
        required_energy = max(attack.energy, 200)

        if mobile.energy >= required_energy && !mobile.casting do
          Ability.execute(room, mobile.ref, attack, [target_ref])
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
      !is_nil(room.zone_controller_id) && room_exit["zone"] == room.zone_controller_id &&
        passable?(room, room_exit, mobile)
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

  defp should_move?(%ApathyDrive.Character{auto_roam: true} = character, room) do
    cond do
      !is_nil(Mobile.auto_attack_target(character, room)) ->
        :fighting

      character.hp < 0.8 ->
        :hp

      character.mana < 0.5 ->
        :mana

      character.energy < character.max_energy ->
        :energy

      :else ->
        true
    end
  end

  defp should_move?(%{movement: "stationary"}, _room), do: false

  defp should_move?(%{auto_roam: true} = mobile, room) do
    not_attacking? = is_nil(Mobile.auto_attack_target(mobile, room))
    roll? = :rand.uniform(10000) > 9990

    not_attacking? and roll?
  end

  defp should_move?(%{} = _mobile, _room), do: false

  defp should_flee?(%Monster{movement: "stationary"}, _room), do: false

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
      Enum.reject(abilities, &(&1.targets in ["self", "weapon"]))
    else
      abilities
    end
  end

  defp reject_item_if_monster(abilities, %Monster{}) do
    Enum.reject(abilities, &(&1.targets in ["armour", "weapon", "item"]))
  end

  defp reject_item_if_monster(abilities, %{}), do: abilities

  defp reject_elemental_if_no_lore(abilities, mobile) do
    if !Map.get(mobile, :lore) do
      Enum.reject(abilities, & &1.traits["Elemental"])
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
      Enum.reject(
        abilities,
        &((!is_nil(&1.traits["Light"]) and &1.traits["Light"] > 0) or
            (!is_nil(&1.traits["DarkVision"]) and &1.traits["DarkVision"] > 0))
      )
    else
      abilities
    end
  end
end
