defmodule ApathyDrive.Regeneration do
  alias ApathyDrive.{Ability, AI, Mobile, Monster, Room}

  @ticks_per_round 20
  @round_length 5000

  def tick_time(_mobile), do: trunc(@round_length / @ticks_per_round)

  def round_length, do: @round_length

  def duration_for_energy(mobile, energy) do
    regen_per_tick = energy_per_tick(mobile)

    ticks_for_energy = energy / regen_per_tick

    trunc(tick_time(mobile) * ticks_for_energy)
  end

  def regenerate(mobile, room) do
    now = DateTime.utc_now()

    mobile
    |> regenerate_energy()
    |> regenerate_hp(room)
    |> regenerate_bubble()
    |> regenerate_mana(room)
    |> Map.put(:last_tick_at, now)
    |> Mobile.update_prompt(room)
  end

  def energy_per_tick(mobile) do
    energy = mobile.max_energy / @ticks_per_round

    speed = Mobile.ability_value(mobile, "Speed")

    modifier = if speed == 0, do: 1, else: speed

    trunc(energy * (1 / modifier))
  end

  def energy_since_last_tick(%{last_tick_at: nil} = mobile), do: energy_per_tick(mobile)

  def energy_since_last_tick(%{last_tick_at: last_tick} = mobile) do
    ms_since_last_tick = DateTime.diff(DateTime.utc_now(), last_tick, :millisecond)

    energy_per_tick = energy_per_tick(mobile)
    energy = energy_per_tick * ms_since_last_tick / tick_time(mobile)

    min(energy, energy_per_tick)
  end

  def hp_since_last_tick(room, %{last_tick_at: nil} = mobile) do
    hp_per_tick = regen_per_tick(room, mobile, Mobile.hp_regen_per_30(mobile))

    hp_percent_per_tick = hp_per_tick / Mobile.max_hp_at_level(mobile, mobile.level)

    hp_percent_per_tick =
      if healing_rune_present?(room) do
        # 1% per second
        hp_percent_per_tick + 0.01 / (1000 / tick_time(mobile))
      else
        hp_percent_per_tick
      end

    heal_per_tick = heal_effect_per_tick(mobile)
    damage_per_tick = damage_effect_per_tick(mobile)

    hp_percent_per_tick + heal_per_tick - damage_per_tick
  end

  def hp_since_last_tick(room, %{last_tick_at: last_tick} = mobile) do
    ms_since_last_tick = DateTime.diff(DateTime.utc_now(), last_tick, :millisecond)

    hp_per_tick = regen_per_tick(room, mobile, Mobile.hp_regen_per_30(mobile))

    hp_percent_per_tick = hp_per_tick / Mobile.max_hp_at_level(mobile, mobile.level)

    hp_percent_per_tick =
      if healing_rune_present?(room) do
        # 1% per second
        hp_percent_per_tick + 0.01 / (1000 / tick_time(mobile))
      else
        hp_percent_per_tick
      end

    heal_per_tick = heal_effect_per_tick(mobile)
    damage_per_tick = damage_effect_per_tick(mobile)

    total_hp_per_tick = hp_percent_per_tick + heal_per_tick - damage_per_tick

    hp = total_hp_per_tick * ms_since_last_tick / tick_time(mobile)

    min(hp, total_hp_per_tick)
  end

  def bubble_since_last_tick(%{last_tick_at: nil} = mobile, bubble_per_second) do
    bubble_per_second / (1000 / tick_time(mobile)) / 100
  end

  def bubble_since_last_tick(%{last_tick_at: last_tick} = mobile, bubble_per_second) do
    ms_since_last_tick = DateTime.diff(DateTime.utc_now(), last_tick, :millisecond)

    bubble_per_tick = bubble_per_second / (1000 / tick_time(mobile)) / 100

    bubble = bubble_per_tick * ms_since_last_tick / tick_time(mobile)

    min(bubble, bubble_per_tick)
  end

  def heal_effect_per_tick(%{} = mobile) do
    Mobile.ability_value(mobile, "Heal") / @ticks_per_round
  end

  def damage_effect_per_tick(%{} = mobile) do
    Mobile.ability_value(mobile, "Damage") / @ticks_per_round
  end

  def mana_since_last_tick(room, %{last_tick_at: nil} = mobile) do
    mana_per_tick = regen_per_tick(room, mobile, Mobile.mana_regen_per_30(mobile))

    mana_percent_per_tick = mana_per_tick / Mobile.max_mana_at_level(mobile, mobile.level)

    if healing_rune_present?(room) do
      # 1% per second
      mana_percent_per_tick + 0.01 / (1000 / tick_time(mobile))
    else
      mana_percent_per_tick
    end
  end

  def mana_since_last_tick(room, %{last_tick_at: last_tick} = mobile) do
    ms_since_last_tick = DateTime.diff(DateTime.utc_now(), last_tick, :millisecond)
    mana_per_tick = regen_per_tick(room, mobile, Mobile.mana_regen_per_30(mobile))

    mana_percent = mana_per_tick / Mobile.max_mana_at_level(mobile, mobile.level)

    mana_percent =
      if healing_rune_present?(room) do
        # 1% per second
        mana_percent + 0.01 / (1000 / tick_time(mobile))
      else
        mana_percent
      end

    mana = mana_percent * ms_since_last_tick / tick_time(mobile)

    min(mana, mana_per_tick)
  end

  def regenerate_energy(mobile) do
    energy = energy_since_last_tick(mobile)

    update_in(
      mobile,
      [Access.key!(:energy)],
      &min(mobile.max_energy, &1 + energy)
    )
  end

  def regenerate_hp(%{} = mobile, room) do
    percentage = hp_since_last_tick(room, mobile)

    if percentage < 0 do
      {mobile, percentage} =
        mobile.effects
        |> Enum.reduce({mobile, percentage}, fn
          {id, %{"Bubble" => bubble} = effect}, {mobile, percentage} ->
            cond do
              bubble > abs(percentage) ->
                mobile = update_in(mobile.effects[id]["Bubble"], &(&1 + percentage))
                {mobile, 0}

              bubble <= abs(percentage) ->
                if effect["MaxBubble"] do
                  mobile = put_in(mobile.effects[id]["Bubble"], 0)
                  # target = Map.put(target, :effects, effects)
                  {mobile, percentage + bubble}
                else
                  mobile = Systems.Effect.remove(mobile, id, show_expiration_message: true)

                  {mobile, percentage + bubble}
                end
            end

          {_id, _effect}, {mobile, percentage} ->
            {mobile, percentage}
        end)

      Mobile.shift_hp(mobile, percentage)
    else
      Mobile.shift_hp(mobile, percentage)
    end
  end

  def regenerate_bubble(%{} = mobile) do
    effects =
      mobile.effects
      |> Enum.reduce(mobile.effects, fn
        {id, %{"Bubble" => bubble, "MaxBubble" => max, "BubbleRegen%PerSecond" => rate}}, effects
        when max > bubble ->
          percentage = bubble_since_last_tick(mobile, rate)
          remaining = max - bubble

          cond do
            remaining >= percentage ->
              effects = update_in(effects[id]["Bubble"], &(&1 + percentage))
              effects

            remaining < percentage ->
              effects = put_in(effects[id]["Bubble"], max)
              effects
          end

        {_id, _effect}, effects ->
          effects
      end)

    Map.put(mobile, :effects, effects)
  end

  def regenerate_mana(%{mana: 1.0} = mobile, _room), do: mobile

  def regenerate_mana(%{} = mobile, room) do
    mana = mana_since_last_tick(room, mobile)

    update_in(mobile, [Access.key!(:mana)], &min(1.0, &1 + mana))
  end

  def regen_per_tick(_room, %{} = _mobile, regen_per_second) do
    ticks_per_30_second = @ticks_per_round * (:timer.seconds(30) / @round_length)
    regen_per_second / ticks_per_30_second
  end

  def healing_rune_present?(room) do
    !!Enum.find(room.items, &(&1.id == ApathyDrive.Scripts.HealingRune.item_id()))
  end

  def use_rest_rate?(room, %Monster{} = mobile) do
    if owner = AI.owner(room, mobile) do
      use_rest_rate?(room, owner)
    else
      false
    end
  end

  def use_rest_rate?(_room, %{} = mobile) do
    !!Map.get(mobile, :resting)
  end

  def heal_limbs(room, target_ref, percentage \\ nil) do
    Room.update_mobile(room, target_ref, fn room, target ->
      percentage = percentage || hp_since_last_tick(room, target)

      if Map.has_key?(target, :limbs) do
        limbs =
          target.limbs
          |> Map.keys()
          |> Enum.filter(&(target.limbs[&1].health > 0 and target.limbs[&1].health < 1.0))

        Enum.reduce(limbs, room, fn limb, room ->
          percentage = percentage * 2 / length(limbs)

          heal_limb(room, target_ref, percentage, limb)
        end)
      else
        target
      end
    end)
  end

  def heal_limb(room, target_ref, percentage, limb) do
    Room.update_mobile(room, target_ref, fn room, target ->
      if Map.has_key?(target, :limbs) do
        initial_limb_health = target.limbs[limb].health

        target =
          update_in(
            target.limbs[limb].health,
            &min(1.0, &1 + percentage)
          )

        limb_health = target.limbs[limb].health

        if initial_limb_health < 0.5 and limb_health >= 0.5 and !target.limbs[limb].fatal do
          Mobile.send_scroll(target, "<p>Your #{limb} is no longer crippled!</p>")

          Room.send_scroll(
            room,
            "<p>#{Mobile.colored_name(target)}'s #{limb} is no longer crippled!</p>",
            [target]
          )

          Systems.Effect.remove_oldest_stack(target, {:crippled, limb})
        else
          target
        end
      else
        target
      end
    end)
  end

  def balance_limbs(room, target_ref) do
    Room.update_mobile(room, target_ref, fn room, target ->
      healthiest_limb =
        target.limbs
        |> Map.keys()
        |> Enum.shuffle()
        |> Enum.filter(&(target.limbs[&1].health > 0))
        |> Enum.sort_by(&{target.limbs[&1].fatal, -target.limbs[&1].health})
        |> List.first()

      target.limbs
      |> Enum.reduce(room, fn {limb_name, _limb}, room ->
        Room.update_mobile(room, target_ref, fn room, target ->
          limb = target.limbs[limb_name]

          cond do
            target.hp < 0 and limb_name == healthiest_limb ->
              Mobile.send_scroll(
                target,
                "<p><span class='dark-red'>You are bleeding!</span></p>"
              )

              Room.send_scroll(
                room,
                "<p><span class='dark-red'>#{Mobile.colored_name(target)} is bleeding!</span></p>",
                [target]
              )

              amount = max(0.10, 1 / Mobile.max_hp_at_level(target, target.level))

              room
              |> Room.update_mobile(target_ref, fn _room, target ->
                update_in(target, [:hp], &(&1 + amount))
              end)
              |> Ability.damage_limb(target_ref, healthiest_limb, -amount * 2, true)

            is_nil(limb[:parent]) ->
              target

            limb.health < 0 ->
              Mobile.send_scroll(
                target,
                "<p><span class='dark-red'>You are bleeding!</span></p>"
              )

              Room.send_scroll(
                room,
                "<p><span class='dark-red'>#{Mobile.colored_name(target)} is bleeding!</span></p>",
                [target]
              )

              max_hp = Mobile.max_hp_at_level(target, target.level)

              if max_hp > 0 do
                hp_amount = max(0.1, 1 / max_hp)
                limb_amount = min(hp_amount * 2, abs(target.limbs[limb_name].health))

                room =
                  room
                  |> heal_limb(target.ref, limb_amount, limb_name)
                  |> update_in([:mobiles, target.ref, :hp], &(&1 - hp_amount))

                Room.update_hp_bar(room, target.ref)
                room
              else
                room
              end

            :else ->
              target
          end
        end)
      end)
    end)
  end
end
