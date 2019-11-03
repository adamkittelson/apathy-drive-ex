defmodule ApathyDrive.Aggression do
  alias ApathyDrive.{Character, Companion, Mobile, Monster, Room}
  require Logger

  def react(%Room{} = room, monster_ref) do
    if room.mobiles[monster_ref] do
      Enum.reduce(room.mobiles, room, fn
        # don't consider attacking self
        {^monster_ref, %{} = _mobile}, updated_room ->
          updated_room

        {_ref, %{} = mobile}, updated_room ->
          monster = updated_room.mobiles[monster_ref]

          if enemy?(monster, mobile) or attacking_owner?(monster, mobile, room) do
            attack(updated_room, monster, mobile)
          else
            updated_room
          end
      end)
    else
      room
    end
  end

  def attacking_owner?(%Monster{owner_id: nil}, _mobile, _room), do: false

  def attacking_owner?(%Monster{owner_id: id}, mobile, room) do
    owner =
      room.mobiles
      |> Map.values()
      |> Enum.find(fn mob ->
        (Map.get(mob, :room_monster_id) || Map.get(mob, :id)) == id
      end)

    if owner do
      case mobile do
        %Character{attack_target: target} ->
          target == owner.ref

        %Monster{} = monster ->
          owner.ref in Monster.enemies(monster, room)

        %Companion{} = monster ->
          owner.ref in Companion.enemies(monster, room)
      end
    else
      false
    end
  end

  def enemy?(%Monster{npc: true}, %{} = _mobile) do
    false
  end

  def enemy?(%Monster{alignment: "neutral"}, %{} = _mobile) do
    false
  end

  def enemy?(%Monster{owner_id: id}, _) when not is_nil(id), do: false

  def enemy?(%Monster{alignment: "good", lawful: true}, %Character{} = character) do
    if Character.legal_status(character) in ["Outlaw", "Criminal", "Villain", "FIEND"] do
      true
    else
      false
    end
  end

  def enemy?(%Monster{alignment: "good"}, %Character{}) do
    false
  end

  def enemy?(%Monster{alignment: "evil", lawful: true}, %Character{} = character) do
    if Character.alignment(character) == "evil", do: false, else: true
  end

  def enemy?(%Monster{alignment: "evil", lawful: true}, %Monster{alignment: "evil", lawful: false}) do
    true
  end

  def enemy?(%Monster{alignment: "evil"}, %Character{}) do
    true
  end

  def enemy?(%Monster{} = monster, %{alignment: "evil"} = mob) do
    cond do
      monster.alignment == "good" ->
        !Mobile.ability_value(mob, "Alignment")

      monster.spawned_at == Map.get(mob, :spawned_at) ->
        false

      monster.zone_spawned_at == Map.get(mob, :zone_spawned_at) ->
        false

      is_nil(monster.owner_id) and is_nil(Map.get(mob, :owner_id)) ->
        false

      monster.alignment == "evil" and !monster.lawful ->
        true

      :else ->
        false
    end
  end

  def enemy?(%Monster{} = monster, %{} = _mob) do
    cond do
      monster.alignment == "evil" ->
        true

      :else ->
        false
    end
  end

  def enemy?(%{attack_target: target} = _monster, %{ref: ref} = _mob) when ref == target, do: true
  def enemy?(%{} = _monster, %{} = _mob), do: false

  def attack(%Room{} = room, %{} = attacker, %{} = intruder) do
    attacker = attack_target(attacker, intruder)

    room = put_in(room.mobiles[attacker.ref], attacker)

    Room.update_hp_bar(room, attacker.ref)
    Room.update_mana_bar(room, attacker.ref)

    room.mobiles
    |> Enum.reduce(room, fn {_ref, mobile}, room ->
      if owner_id = Map.get(mobile, :owner_id) do
        if owner_id == (Map.get(attacker, :room_monster_id) || Map.get(attacker, :id)) do
          attack(room, mobile, intruder)
        else
          room
        end
      else
        room
      end
    end)
  end

  def attack_target(%{} = attacker, %{ref: ref} = _intruder) do
    effect = %{"Aggro" => ref, "stack_key" => {:aggro, ref}, "stack_count" => 1}

    attacker =
      attacker
      |> Systems.Effect.add(effect, 60_000)

    if Map.has_key?(attacker, :attack_target) and attacker.attack_target == nil do
      attacker
      |> Map.put(:attack_target, ref)
      |> Mobile.send_scroll("<p><span class='dark-yellow'>*Combat Engaged*</span></p>")
    else
      attacker
    end
  end

  def enemies_present?(room, character) do
    room.mobiles
    |> Map.values()
    |> Enum.any?(&enemy?(&1, character))
  end
end
