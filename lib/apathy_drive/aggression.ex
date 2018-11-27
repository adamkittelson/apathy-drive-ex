defmodule ApathyDrive.Aggression do
  alias ApathyDrive.{Character, Mobile, Monster, Room}
  require Logger

  def react(%Room{} = room, monster_ref) do
    Enum.reduce(room.mobiles, room, fn
      # don't consider attacking self
      {^monster_ref, %{} = _mobile}, updated_room ->
        updated_room

      {_ref, %{} = mobile}, updated_room ->
        monster = updated_room.mobiles[monster_ref]

        ApathyDrive.Aggression.react(room, monster, mobile)
    end)
  end

  def react(%Room{} = room, %Monster{alignment: "neutral"}, %{} = _mobile) do
    room
  end

  def react(
        %Room{} = room,
        %Monster{alignment: "good", lawful: true} = monster,
        %Character{} = character
      ) do
    IO.inspect(Character.legal_status(character))

    if Character.legal_status(character) in ["Outlaw", "Criminal", "Villain", "FIEND"] do
      IO.puts("#{monster.name} attacking #{character.name}")
      attack(room, monster, character)
    else
      room
    end
  end

  def react(
        %Room{} = room,
        %Monster{alignment: "good"},
        %Character{}
      ) do
    room
  end

  def react(
        %Room{} = room,
        %Monster{alignment: "evil", lawful: true},
        %Character{alignment: "evil"}
      ) do
    room
  end

  def react(
        %Room{} = room,
        %Monster{alignment: "evil"} = monster,
        %Character{} = character
      ) do
    attack(room, monster, character)
  end

  def react(%Room{} = room, %Monster{} = monster, %{alignment: "evil"} = mob) do
    cond do
      monster.alignment == "good" ->
        attack(room, monster, mob)

      monster.spawned_at == Map.get(mob, :spawned_at) ->
        room

      monster.alignment == "evil" and !monster.lawful ->
        attack(room, monster, mob)

      :else ->
        room
    end
  end

  def react(%Room{} = room, %Monster{} = monster, %{} = mob) do
    cond do
      monster.alignment == "evil" ->
        attack(room, monster, mob)

      :else ->
        room
    end
  end

  def attack(%Room{} = room, %{} = attacker, %{} = intruder) do
    attacker = attack_target(attacker, intruder)

    room = put_in(room.mobiles[attacker.ref], attacker)

    Room.update_hp_bar(room, attacker.ref)
    Room.update_mana_bar(room, attacker.ref)

    room
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
end
