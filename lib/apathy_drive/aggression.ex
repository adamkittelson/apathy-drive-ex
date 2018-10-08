defmodule ApathyDrive.Aggression do
  alias ApathyDrive.{Character, Mobile, Monster, Room, TimerManager}
  require Logger

  def react(%Room{} = room, monster_ref) do
    Enum.reduce(room.mobiles, room, fn
      {_ref, %Monster{}}, updated_room ->
        updated_room

      {_ref, %{} = mobile}, updated_room ->
        monster = updated_room.mobiles[monster_ref]
        put_in(updated_room.mobiles[monster_ref], ApathyDrive.Aggression.react(monster, mobile))
    end)
  end

  # Don't attack other monsters
  def react(%Monster{} = monster, %Monster{}), do: monster

  # attack non-monsters if hostile
  def react(%Monster{hostile: true} = monster, %{} = intruder) do
    attack(monster, intruder)
  end

  def react(%Monster{} = monster, %{} = _intruder), do: monster

  def react(%{} = mobile, %{}), do: mobile

  def attack(%{} = attacker, %{ref: ref} = _intruder) do
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
      |> Character.update_hp_bar()
      |> Character.update_mana_bar()
    else
      attacker
    end
  end
end
