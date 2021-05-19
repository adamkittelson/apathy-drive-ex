defmodule ApathyDrive.Regeneration do
  alias ApathyDrive.{AI, Character, Mobile, Monster}

  @ticks_per_round 20
  @round_length 4000

  def tick_time(mobile) do
    trunc(round_length(mobile) / @ticks_per_round)
  end

  def round_length(mobile) do
    speed = Mobile.ability_value(mobile, "Speed")

    modifier = if speed == 0, do: 1, else: speed

    trunc(@round_length * modifier)
  end

  def per_tick_to_per_round(amount) do
    amount * @ticks_per_round
  end

  def per_round_to_per_30(amount, mobile) do
    amount * (:timer.seconds(30) / round_length(mobile))
  end

  def per_tick_to_per_30(amount, mobile) do
    amount
    |> per_tick_to_per_round()
    |> per_round_to_per_30(mobile)
  end

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
    |> regenerate_powerstones()
    |> regenerate_mana(room)
    |> Map.put(:last_tick_at, now)
    |> Mobile.update_prompt(room)
  end

  def energy_per_tick(mobile) do
    mobile.max_energy / @ticks_per_round
  end

  def energy_since_last_tick(%{last_tick_at: nil} = mobile), do: energy_per_tick(mobile)

  def energy_since_last_tick(%{last_tick_at: last_tick} = mobile) do
    ms_since_last_tick = DateTime.diff(DateTime.utc_now(), last_tick, :millisecond)

    energy_per_tick = energy_per_tick(mobile)
    energy = energy_per_tick * ms_since_last_tick / tick_time(mobile)

    min(energy, energy_per_tick)
  end

  def hp_since_last_tick(room, %{last_tick_at: nil} = mobile) do
    hp_per_tick = regen_per_tick(Mobile.hp_regen_per_30(mobile))

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

    hp_per_tick = regen_per_tick(Mobile.hp_regen_per_30(mobile))

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
    mana_per_tick = regen_per_tick(Mobile.mana_regen_per_30(mobile))

    if mana_per_tick > 0 do
      mana_percent_per_tick = mana_per_tick / Mobile.max_mana_at_level(mobile, mobile.level)

      if healing_rune_present?(room) do
        # 1% per second
        mana_percent_per_tick + 0.01 / (1000 / tick_time(mobile))
      else
        mana_percent_per_tick
      end
    else
      0
    end
  end

  def mana_since_last_tick(room, %{last_tick_at: last_tick} = mobile) do
    ms_since_last_tick = DateTime.diff(DateTime.utc_now(), last_tick, :millisecond)
    mana_per_tick = regen_per_tick(Mobile.mana_regen_per_30(mobile))

    if mana_per_tick > 0 do
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
    else
      0
    end
  end

  def regenerate_powerstones(%Character{} = character) do
    # 1% per second
    percentage = 0.01 * (tick_time(character) / 1000)

    Enum.reduce(character.inventory, character, fn item, character ->
      if "create powerstone" in item.enchantments do
        character = update_in(character.inventory, &List.delete(&1, item))
        item = update_in(item.uses, &min(item.max_uses, &1 + item.max_uses * percentage))
        update_in(character.inventory, &[item | &1])
      else
        character
      end
    end)
  end

  def regenerate_powerstones(%{} = mobile), do: mobile

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

  def regen_per_tick(regen_per_second) do
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
end
