defmodule ApathyDrive.Aggression do
  alias ApathyDrive.{Mobile, Monster, Room}
  require Logger

  def react(%Room{} = room, monster_ref) do
    Enum.reduce(room.mobiles, room, fn
      # don't consider attacking self
      {^monster_ref, %{} = mobile}, updated_room ->
        updated_room

      {_ref, %{} = mobile}, updated_room ->
        monster = updated_room.mobiles[monster_ref]

        alignment =
          mobile
          |> Mobile.alignment(room)
          |> convert_alignment()

        ApathyDrive.Aggression.react(room, monster, %{alignment: alignment, mobile: mobile})
    end)
  end

  # monsters with neutral dispositions never initiate combat
  def react(%Room{} = room, %Monster{disposition: "neutral"}, _), do: room

  def react(%Room{} = room, %Monster{} = monster, %{alignment: "evil", mobile: mob}) do
    cond do
      monster.alignment == "good" ->
        attack(room, monster, mob)

      monster.spawned_at == Map.get(mob, :spawned_at) ->
        room

      monster.alignment == "evil" and monster.disposition != "lawful" ->
        attack(room, monster, mob)

      :else ->
        room
    end
  end

  def react(%Room{} = room, %Monster{} = monster, %{alignment: _alignment, mobile: mob}) do
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

  def convert_alignment("Saint"), do: "good"
  def convert_alignment("Good"), do: "good"
  def convert_alignment("Neutral"), do: "neutral"
  def convert_alignment("Seedy"), do: "neutral"
  def convert_alignment("Outlaw"), do: "evil"
  def convert_alignment("Criminal"), do: "evil"
  def convert_alignment("Villain"), do: "evil"
  def convert_alignment("FIEND"), do: "evil"
  def convert_alignment(alignment), do: alignment
end
