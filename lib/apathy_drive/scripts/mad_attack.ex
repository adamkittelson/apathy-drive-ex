defmodule ApathyDrive.Scripts.MadAttack do
  alias ApathyDrive.{Ability, Mobile, Room}

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    rounds = Enum.random(2..7)

    Enum.reduce(1..rounds, room, fn _round, room ->
      mobile = room.mobiles[mobile_ref]

      if mobile do
        target = room.mobiles[Mobile.auto_attack_target(mobile, room)]

        if target do
          Mobile.send_scroll(
            mobile,
            "<p><span class='yellow'>You attack #{target.name} in a crazed, magic induced rage!</span></p>"
          )

          Mobile.send_scroll(
            target,
            "<p><span class='yellow'>#{mobile.name} attacks you in a crazed, magic induced rage!</span></p>"
          )

          Room.send_scroll(
            room,
            "<p><span class='yellow'>#{mobile.name} attacks #{target.name} in a crazed, magic induced rage!</span></p>",
            [mobile, target]
          )

          mobile = Map.put(mobile, :energy, mobile.max_energy)
          room = put_in(room.mobiles[mobile_ref].energy, mobile.max_energy)

          attack_round(mobile, room) || room
        else
          room
        end
      else
        room
      end
    end)
  end

  def attack_round(mobile, room) do
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
end
