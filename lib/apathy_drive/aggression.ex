defmodule ApathyDrive.Aggression do
  alias ApathyDrive.{Character, Mobile, Monster, Room, TimerManager}
  require Logger

  def react(%Room{} = room, monster_ref) do
    Enum.reduce(room.mobiles, room, fn
      {_ref, %Monster{}}, updated_room ->
        updated_room

      {_ref, %{} = mobile}, updated_room ->
        monster = updated_room.mobiles[monster_ref]
        ApathyDrive.Aggression.react(room, monster, mobile)
    end)
  end

  # Don't attack other monsters
  def react(%Room{} = room, %Monster{}, %Monster{}), do: room

  # attack non-monsters if hostile
  def react(%Room{} = room, %Monster{hostile: true} = monster, %{} = intruder) do
    attack(room, monster, intruder)
  end

  def react(%Room{} = room, %Monster{} = _monster, %{} = _intruder), do: room

  def react(%Room{} = room, %{} = _mobile, %{}), do: room

  def attack(%Room{} = room, %{} = attacker, %{} = intruder) do
    attacker = attack_target(attacker, intruder)

    room = put_in(room.mobiles[attacker.ref], attacker)

    Room.update_hp_bar(room, attacker.ref)

    room
  end

  def attack_target(%{} = attacker, %{ref: ref} = _intruder) do
    effect = %{"Aggro" => ref, "stack_key" => {:aggro, ref}, "stack_count" => 1}

    time = max(0, TimerManager.time_remaining(attacker, :auto_attack_timer))

    attacker =
      attacker
      |> Systems.Effect.add(effect, 60_000)
      |> TimerManager.send_after({:auto_attack_timer, time, {:execute_auto_attack, attacker.ref}})

    if Map.has_key?(attacker, :attack_target) and attacker.attack_target == nil do
      attacker
      |> Map.put(:attack_target, ref)
      |> Mobile.send_scroll("<p><span class='dark-yellow'>*Combat Engaged*</span></p>")
      |> Character.update_mana_bar()
    else
      attacker
    end
  end
end
