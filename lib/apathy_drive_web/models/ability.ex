defmodule ApathyDrive.Ability do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Ability, AbilityTrait, Character, Companion, Match, Mobile, Monster, Party, Room, Stealth, Text, TimerManager}
  require Logger

  schema "abilities" do
    field :name, :string
    field :targets, :string
    field :kind, :string
    field :mana, :integer, default: 0
    field :command, :string
    field :description, :string
    field :user_message, :string
    field :target_message, :string
    field :spectator_message, :string
    field :duration, :integer, default: 0
    field :cooldown, :integer
    field :cast_time, :integer

    field :level, :integer, virtual: true
    field :traits, :map, virtual: true, default: %{}
    field :ignores_round_cooldown?, :boolean, virtual: true, default: false
    field :result, :any, virtual: true
    field :cast_complete, :boolean, virtual: true, default: false

    has_many :monsters_abilities, ApathyDrive.MonsterAbility
    has_many :monsters, through: [:monsters_abilities, :monster]

    has_many :abilities_traits, ApathyDrive.AbilityTrait
    has_many :trait_records, through: [:abilities_traits, :trait]

    timestamps()
  end

  @required_fields ~w(name targets kind mana command description user_message target_message spectator_message duration)
  @optional_fields ~w()

  @valid_targets ["monster or single", "self", "self or single", "monster", "full party area", "full attack area", "single", "full area"]
  @target_required_targets ["monster or single", "monster", "single"]

  @kinds ["heal", "attack", "auto attack", "curse", "utility", "blessing", "passive"]

  @instant_traits [
    "CurePoison", "Damage", "DispelMagic", "Drain", "Enslave", "Freedom", "Heal", "HealMana", "KillSpell",
    "MagicalDamage", "PhysicalDamage", "Poison", "RemoveSpells", "Script", "Summon", "Teleport"
  ]

  @duration_traits [
    "AC", "Accuracy", "Agility", "Charm", "Blind", "Charm", "Confusion", "ConfusionMessage", "ConfusionSpectatorMessage",
    "Crits", "DamageShield", "DamageShieldUserMessage", "DamageShieldTargetMessage", "DamageShieldSpectatorMessage",
    "DamageType", "Dodge", "Encumbrance", "EndCast", "EndCast%", "EnhanceSpell", "EnhanceSpellDamage", "Fear", "Heal", "Health", "HPRegen",
    "Intellect", "Light", "MagicalDamage", "MagicalResist", "ManaRegen", "MaxHP", "MaxMana", "ModifyDamage", "Perception", "Picklocks", "PhysicalDamage",
    "PoisonImmunity", "RemoveMessage", "ResistCold", "ResistFire", "ResistLightning", "ResistStone", "Root", "SeeHidden",
    "Shadowform", "Silence", "Speed", "Spellcasting", "StatusMessage", "Stealth", "Strength", "Tracking", "Willpower"
  ]

  @resist_message %{
    user: "You attempt to cast {{ability}} on {{target}}, but they resist!",
    target: "{{user}} attempts to cast {{ability}} on you, but you resist!",
    spectator: "{{user}} attempts to cast {{ability}} on {{target}}, but they resist!"
  }

  @deflect_message %{
    user: "{{target}}'s armour deflects your feeble attack!",
    target: "Your armour deflects {{user}}'s feeble attack!",
    spectator: "{{target}}'s armour deflects {{user}}'s feeble attack!"
  }

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  def changeset(model, params \\ %{}) do
    model
    |> cast(params, @required_fields, @optional_fields)
  end

  def set_description_changeset(model, description) do
    model
    |> cast(%{description: description}, [:description])
    |> validate_required(:description)
    |> validate_length(:description, min: 20, max: 500)
  end

  def set_duration_changeset(model, duration) do
    model
    |> cast(%{duration: duration}, [:duration])
    |> validate_required(:duration)
    |> validate_number(:duration, greater_than: 0)
  end

  def set_mana_changeset(model, mana) do
    model
    |> cast(%{mana: mana}, [:mana])
    |> validate_required(:mana)
    |> validate_number(:mana, greater_than: 0)
  end

  def set_cast_time_changeset(model, cast_time) do
    model
    |> cast(%{cast_time: cast_time}, [:cast_time])
    |> validate_required(:cast_time)
    |> validate_number(:cast_time, greater_than_or_equal_to: 0)
  end

  def set_user_message_changeset(model, message) do
    model
    |> cast(%{user_message: message}, [:user_message])
    |> validate_required(:user_message)
  end

  def set_command_changeset(model, command) do
    model
    |> cast(%{command: command}, [:command])
    |> validate_required(:command)
    |> validate_length(:command, min: 3, max: 10)
  end

  def set_target_message_changeset(model, message) do
    model
    |> cast(%{target_message: message}, [:target_message])
    |> validate_required(:target_message)
  end

  def set_spectator_message_changeset(model, message) do
    model
    |> cast(%{spectator_message: message}, [:spectator_message])
    |> validate_required(:spectator_message)
  end

  def set_targets_changeset(model, targets) do
    model
    |> cast(%{targets: targets}, [:targets])
    |> validate_required(:targets)
    |> validate_inclusion(:targets, @valid_targets)
  end

  def set_kind_changeset(model, kind) do
    model
    |> cast(%{kind: kind}, [:kind])
    |> validate_required(:kind)
    |> validate_inclusion(:kind, @kinds)
  end

  def find(id) do
    ability = ApathyDrive.Repo.get(__MODULE__, id)

    if ability do
      put_in(ability.traits, AbilityTrait.load_traits(id))
    end
  end

  def match_by_name(name, all \\ false) do
    abilities =
      __MODULE__
      |> where([ability], not is_nil(ability.name) and ability.name != "")
      |> distinct(true)
      |> ApathyDrive.Repo.all

      if all do
        Match.all(abilities, :keyword_starts_with, name)
      else
        Match.one(abilities, :keyword_starts_with, name)
      end

  end

  def heal_abilities(%{abilities: abilities} = _mobile) do
    abilities
    |> Map.values
    |> Enum.filter(&(&1.kind == "heal"))
  end

  # equivilent to a character with 2 ManaPerIntellect and 10 intellect
  def base_mana_at_level(level), do: 20 + ((level - 1) * 2)

  def mana_cost_at_level(%Ability{mana: mana} = _ability, level) do
    trunc(base_mana_at_level(level) * (mana / 100))
  end

  def execute(%Room{} = room, caster_ref, %Ability{cast_time: time} = ability, query) when not is_nil(time) do
    Room.update_mobile(room, caster_ref, fn mobile ->
      if TimerManager.time_remaining(mobile, :casting) > 0 do
        Mobile.send_scroll(mobile, "<p><span class='dark-red'>You interrupt your other spell.</span></p>")
      end
      Mobile.send_scroll(mobile, "<p><span class='cyan'>You begin your casting.</span></p>")
      ability =
        ability
        |> Map.put(:cast_time, nil)
        |> Map.put(:cast_complete, true)

      TimerManager.send_after(mobile, {:casting, :timer.seconds(time), {:execute_ability, %{caster: caster_ref, ability: ability, target: query}}})
    end)
  end

  def execute(%Room{} = room, caster_ref, %Ability{targets: targets}, "") when targets in @target_required_targets do
    room
    |> Room.get_mobile(caster_ref)
    |> Mobile.send_scroll("<p><span class='red'>You must specify a target for that ability.</span></p>")

    room
  end

  def execute(%Room{} = room, caster_ref, %Ability{} = ability, query) when is_binary(query) do
    case get_targets(room, caster_ref, ability, query) do
      [] ->
        case query do
          "" ->
            room
            |> Room.get_mobile(caster_ref)
            |> Mobile.send_scroll("<p>Your ability would affect no one.</p>")
          _ ->
            room
            |> Room.get_mobile(caster_ref)
            |> Mobile.send_scroll("<p><span class='cyan'>Can't find #{query} here! Your spell fails.</span></p>")
        end

        room
      targets ->
        execute(room, caster_ref, ability, targets)
    end
  end

  def execute(%Room{} = room, caster_ref, %Ability{} = ability, targets) when is_list(targets) do
    if can_execute?(room, caster_ref, ability) do
      Room.update_mobile(room, caster_ref, fn caster ->
        display_pre_cast_message(room, caster, targets, ability)

        room =
          Enum.reduce(targets, room, fn(target_ref, updated_room) ->
            Room.update_mobile(updated_room, target_ref, fn target ->
              if affects_target?(target, ability) do
                Logger.info "#{caster.name}'s ability affecting #{target.name}"
                updated_room = apply_ability(updated_room, caster, target, ability)

                target = updated_room.mobiles[target_ref]

                if target do
                  target =
                    if ability.kind in ["attack", "curse"] do
                      Stealth.reveal(target, updated_room)
                    else
                      target
                    end

                  if target.hp < 0 do
                    Mobile.die(target, updated_room)
                  else
                    put_in(updated_room.mobiles[target.ref], target)
                  end
                else
                  updated_room
                end
              else
                message = "#{target.name} is not affected by that ability." |> Text.capitalize_first
                Mobile.send_scroll(caster, "<p><span class='cyan'>#{message}</span></p>")
                target
              end
            end)
          end)
          #|> execute_multi_cast(caster_ref, ability, targets)


        room =
          Room.update_mobile(room, caster.ref, fn caster ->
            caster =
              caster
              |> apply_cooldowns(ability)
              |> Mobile.subtract_mana(ability)

            Mobile.update_prompt(caster)

            if ability.kind in ["attack", "curse"] and !(caster.ref in targets) do
              [target_ref | _] = targets

              if Map.has_key?(caster, :attack_target) do
                if is_nil(caster.attack_target) do
                  time = min(Mobile.attack_interval(caster), TimerManager.time_remaining(caster, :auto_attack_timer))

                  caster
                  |> Map.put(:attack_target, target_ref)
                  |> TimerManager.send_after({:auto_attack_timer, time, {:execute_auto_attack, caster.ref}})
                  |> Mobile.send_scroll("<p><span class='dark-yellow'>*Combat Engaged*</span></p>")
                else
                  caster
                  |> Map.put(:attack_target, target_ref)
                end
              else
                caster
              end
            else
              caster
            end

          end)

        Room.update_mobile(room, caster_ref, &Stealth.reveal(&1, room))
      end)
    else
      room
    end
  end

  def duration(%Ability{duration: duration, kind: kind}, %{} = caster, %{} = target, room) do
    caster_level = Mobile.caster_level(caster, target)
    target_level = Mobile.target_level(caster, target)

    caster_sc = Mobile.spellcasting_at_level(caster, caster_level, room)

    if kind == "curse" do
      target_mr = Mobile.magical_resistance_at_level(target, target_level, nil, room)
      trunc(duration * :math.pow(1.005, caster_sc) * :math.pow(0.985, target_mr))
    else
      trunc(duration * :math.pow(1.005, caster_sc))
    end
  end

  def dodged?(%{} = caster, %{} = target, room) do
    caster_level = Mobile.caster_level(caster, target)
    accuracy = Mobile.accuracy_at_level(caster, caster_level, room)

    target_level = Mobile.target_level(caster, target)
    dodge = Mobile.dodge_at_level(target, target_level, room)

    modifier = Mobile.ability_value(target, "Dodge")

    chance = 30 + modifier + ((dodge - accuracy) * 10)

    chance = min(chance, 60 + modifier)
    chance = max(chance, 10 + modifier)

    :rand.uniform(100) < chance
  end

  def apply_ability(%Room{} = room, %{} = caster, %{} = target, %Ability{traits: %{"Dodgeable" => true}} = ability) do
    if dodged?(caster, target, room) do
      display_cast_message(room, caster, target, Map.put(ability, :result, :dodged))

      effects =
        %{"Dodge" => -10}
        |> Map.put("stack_key", "dodge-limiter")
        |> Map.put("stack_count", 5)

      target =
        target
        |> Systems.Effect.add(effects, Mobile.round_length_in_ms(target))
        |> aggro_target(ability, caster)

      put_in(room.mobiles[target.ref], target)
    else
      apply_ability(room, caster, target, update_in(ability.traits, &Map.delete(&1, "Dodgeable")))
    end
  end
  def apply_ability(%Room{} = room, %Character{} = caster, %{} = target, %Ability{traits: %{"Enslave" => _}} = ability) do
    display_cast_message(room, caster, target, ability)

    if companion = Character.companion(caster, room) do
      companion
      |> Companion.dismiss(room)
      |> Companion.convert_for_character(target, caster)
    else
      Companion.convert_for_character(room, target, caster)
    end
  end
  def apply_ability(%Room{} = room, %{} = caster, %{} = target, %Ability{} = ability) do
    {caster, target} =
      target
      |> apply_instant_traits(ability, caster, room)

    target = aggro_target(target, ability, caster)

    room = put_in(room.mobiles[caster.ref], caster)
    room = put_in(room.mobiles[target.ref], target)

    duration = duration(ability, caster, target, room)

    if ability.kind == "curse" and duration < 1000 do
      display_cast_message(room, caster, target, Map.put(ability, :result, :resisted))

      target =
        target
        |> Map.put(:ability_shift, nil)
        |> Map.put(:ability_special, nil)
        |> Mobile.update_prompt

      room
      |> put_in([:mobiles, target.ref], target)
    else
      display_cast_message(room, caster, target, ability)

      room = trigger_damage_shields(room, caster.ref, target.ref, ability)

      target =
        if target.ability_shift do
          Mobile.shift_hp(target, target.ability_shift, room)
        else
          target
        end

      target =
        target
        |> Map.put(:ability_shift, nil)
        |> Map.put(:ability_special, nil)
        |> apply_duration_traits(ability, caster, duration, room)
        |> Mobile.update_prompt

      room
      |> put_in([:mobiles, target.ref], target)
    end
  end

  def trigger_damage_shields(%Room{} = room, caster_ref, target_ref, _ability) when target_ref == caster_ref, do: room
  def trigger_damage_shields(%Room{} = room, caster_ref, target_ref, ability) do
    if (target = room.mobiles[target_ref]) && "PhysicalDamage" in Map.keys(ability.traits) do
      target
      |> Map.get(:effects)
      |> Map.values
      |> Enum.filter(&(Map.has_key?(&1, "DamageShield")))
      |> Enum.reduce(room, fn
           %{"DamageShield" => damage, "DamageType" => damage_type} = shield, updated_room ->
             reaction =
               %Ability{
                 kind: "attack",
                 mana: 0,
                 user_message: shield["DamageShieldUserMessage"],
                 target_message: shield["DamageShieldTargetMessage"],
                 spectator_message: shield["DamageShieldSpectatorMessage"],
                 ignores_round_cooldown?: true,
                 traits: %{
                   "MagicalDamage" => div(ability.traits["PhysicalDamage"] * damage, 100),
                   "DamageType" => damage_type
                 }
               }

             apply_ability(updated_room, room.mobiles[target_ref], room.mobiles[caster_ref], reaction)
         end)
    else
      room
    end
  end

  def aggro_target(%{ref: target_ref} = target, %Ability{kind: kind}, %{ref: caster_ref} = caster) when kind in ["attack", "curse"] and target_ref != caster_ref do
    ApathyDrive.Aggression.attack(target, caster)
  end
  def aggro_target(%{} = target, %Ability{}, %{} = _caster), do: target

  def apply_instant_traits(%{} = target, %Ability{} = ability, %{} = caster, room) do
    ability.traits
    |> Map.take(@instant_traits)
    |> Enum.reduce({caster, target}, fn trait, {updated_caster, updated_target} ->
         apply_instant_trait(trait, updated_target, ability, updated_caster, room)
       end)
  end

  def apply_instant_trait({"RemoveSpells", ability_ids}, %{} = target, _ability, caster, _room) do
    target =
      Enum.reduce(ability_ids, target, fn(ability_id, updated_target) ->
        Systems.Effect.remove_oldest_stack(updated_target, ability_id)
      end)
    {caster, target}
  end
  def apply_instant_trait({"Heal", value}, %{} = target, _ability, caster, _room) when is_float(value) do
    Logger.info "healing #{target.name} #{inspect value}"
    {caster, Map.put(target, :ability_shift, value)}
  end
  def apply_instant_trait({"Damage", value}, %{} = target, _ability, caster, _room) when is_float(value) do
    Logger.info "damaging #{target.name} #{inspect value}"
    {caster, Map.put(target, :ability_shift, -value)}
  end
  def apply_instant_trait({"Heal", value}, %{} = target, _ability, caster, room) do
    level = min(target.level, caster.level)
    healing = Mobile.magical_damage_at_level(caster, level, room) * (value / 100)
    percentage_healed = calculate_healing(healing, value) / Mobile.max_hp_at_level(target, level)

    {caster, Map.put(target, :ability_shift, percentage_healed)}
  end
  def apply_instant_trait({"Drain", value}, %{} = target, ability, caster, room) do
    caster_level = Mobile.caster_level(caster, target)
    target_level = Mobile.target_level(caster, target)

    damage = Mobile.magical_damage_at_level(caster, caster_level, room)
    resist = Mobile.magical_resistance_at_level(target, target_level, ability.traits["DamageType"], room)

    {special, damage} = calculate_damage(damage, resist, value, caster, target, room)

    damage_percent = damage / Mobile.max_hp_at_level(target, target_level)
    heal_percent = damage / Mobile.max_hp_at_level(caster, caster_level)

    target =
      target
      |> Map.put(:ability_shift, -damage_percent)
      |> Map.put(:ability_special, special)

    caster = Mobile.shift_hp(caster, heal_percent, room)

    Mobile.update_prompt(caster)

    {caster, target}
  end
  def apply_instant_trait({"MagicalDamage", value}, %{} = target, ability, caster, room) do
    caster_level = Mobile.caster_level(caster, target)
    target_level = Mobile.target_level(caster, target)

    damage = Mobile.magical_damage_at_level(caster, caster_level, room)
    resist = Mobile.magical_resistance_at_level(target, target_level, ability.traits["DamageType"], room)

    {special, damage} = calculate_damage(damage, resist, value, caster, target, room)

    damage_percent =  damage / Mobile.max_hp_at_level(target, target_level)

    target =
      target
      |> Map.put(:ability_shift, -damage_percent)
      |> Map.put(:ability_special, special)
    {caster, target}
  end
  def apply_instant_trait({"PhysicalDamage", value}, %{} = target, ability, caster, room) do
    caster_level = Mobile.caster_level(caster, target)
    target_level = Mobile.target_level(caster, target)

    damage = Mobile.physical_damage_at_level(caster, caster_level, room)
    resist = Mobile.physical_resistance_at_level(target, target_level, ability.traits["DamageType"], room)

    {special, damage} = calculate_damage(damage, resist, value, caster, target, room)

    damage_percent =  damage / Mobile.max_hp_at_level(target, target_level)

    target =
      target
      |> Map.put(:ability_shift, -damage_percent)
      |> Map.put(:ability_special, special)
    {caster, target}
  end
  def apply_instant_trait({ability_name, _value}, %{} = target, _ability, caster, _room) do
    Mobile.send_scroll(caster, "<p><span class='red'>Not Implemented: #{ability_name}")
    {caster, target}
  end

  def calculate_damage(damage, resist, modifier, caster, target, room) do
    caster_level = Mobile.caster_level(caster, target)
    target_level = Mobile.target_level(caster, target)

    cond do
      surprise?(caster, target, room) ->
        # max modifier to make surprise attacks with fast weapons do a full round's worth of damage
        damage = (damage - resist) * (max(modifier, 100) / 100) * (Enum.random(85..115) / 100)
        {:surprise, damage * 2}
      crit?(caster, caster_level, target, target_level, room) ->
        damage = (damage - resist) * (modifier / 100) * (Enum.random(85..115) / 100)
        {:crit, damage * 2}
      true ->
        damage = (damage - resist) * (modifier / 100) * (Enum.random(85..115) / 100)
        {:normal, damage}
    end
  end

  def surprise?(caster, target, room) do
    Stealth.invisible?(caster, target, room)
  end

  def crit?(caster, caster_level, target, target_level, room) do
    caster_crit = Mobile.crits_at_level(caster, caster_level, room)
    target_crit = Mobile.crits_at_level(target, target_level, room)

    caster_modifier = Mobile.ability_value(caster, "Crits")
    target_modifier = Mobile.ability_value(target, "Crits")

    chance = 30 + caster_modifier - target_modifier + ((caster_crit - target_crit) * 10)

    chance = min(chance, 60 + caster_modifier - target_modifier)
    chance = max(chance, 10 + caster_modifier - target_modifier)

    :rand.uniform(100) < chance
  end

  def calculate_healing(damage, modifier) do
    damage * (modifier / 100) * (Enum.random(95..105) / 100)
  end

  def apply_duration_traits(%{} = target, %Ability{} = ability, %{} = caster, duration, room) do
    effects =
      ability.traits
      |> Map.take(@duration_traits)
      |> Map.put("stack_key", ability.id)
      |> Map.put("stack_count", 1)
      |> process_duration_traits(target, caster, ability, room)
      |> Map.put("effect_ref", make_ref())

    if message = effects["StatusMessage"] do
      Mobile.send_scroll(target, "<p><span class='#{message_color(ability)}'>#{message}</span></p>")
    end

    target
    |> Systems.Effect.add(effects, :timer.seconds(duration))
    |> Systems.Effect.schedule_next_periodic_effect
  end

  def process_duration_traits(effects, target, caster, ability, room) do
    effects
    |> Enum.reduce(effects, fn effect, updated_effects ->
         process_duration_ability(effect, updated_effects, target, caster, ability, room)
       end)
  end

  def process_duration_ability({"PhysicalDamage", modifier}, effects, target, caster, ability, room) do
    caster_level = Mobile.caster_level(caster, target)
    target_level = Mobile.target_level(caster, target)

    damage = Mobile.physical_damage_at_level(caster, caster_level, room)
    resist = Mobile.physical_resistance_at_level(target, target_level, ability.traits["DamageType"], room)

    damage = (damage - resist) * (modifier / 100)

    damage_percent =  damage / Mobile.max_hp_at_level(target, target_level)

    effects
    |> Map.delete("PhysicalDamage")
    |> Map.put("Damage", damage_percent)
    |> Map.put("Interval", Mobile.round_length_in_ms(caster) / 4)
    |> Map.put("NextEffectAt", System.monotonic_time(:milliseconds) + Mobile.round_length_in_ms(caster) / 4)
  end
  def process_duration_ability({"MagicalDamage", modifier}, effects, target, caster, ability, room) do
    caster_level = Mobile.caster_level(caster, target)
    target_level = Mobile.target_level(caster, target)

    damage = Mobile.magical_damage_at_level(caster, caster_level, room)
    resist = Mobile.magical_resistance_at_level(target, target_level, ability.traits["DamageType"], room)

    damage = (damage - resist) * (modifier / 100)

    damage_percent =  damage / Mobile.max_hp_at_level(target, target_level)

    effects
    |> Map.delete("MagicalDamage")
    |> Map.put("Damage", damage_percent)
    |> Map.put("Interval", Mobile.round_length_in_ms(caster) / 4)
    |> Map.put("NextEffectAt", System.monotonic_time(:milliseconds) + Mobile.round_length_in_ms(caster) / 4)
  end
  def process_duration_ability({"Heal", value}, effects, target, caster, _ability, room) do
    level = min(target.level, caster.level)
    healing = Mobile.magical_damage_at_level(caster, level, room) * (value / 100)
    percentage_healed = calculate_healing(healing, value) / Mobile.max_hp_at_level(target, level)

    effects
    |> Map.put("Heal", percentage_healed)
    |> Map.put("Interval", Mobile.round_length_in_ms(caster) / 4)
    |> Map.put("NextEffectAt", System.monotonic_time(:milliseconds) + Mobile.round_length_in_ms(caster) / 4)
  end
  def process_duration_ability({"HealMana", value}, effects, target, caster, _ability, room) do
    level = min(target.level, caster.level)
    healing = Mobile.magical_damage_at_level(caster, level, room) * (value / 100)
    percentage_healed = calculate_healing(healing, value) / Mobile.max_mana_at_level(target, level)

    effects
    |> Map.put("HealMana", percentage_healed)
    |> Map.put("Interval", Mobile.round_length_in_ms(caster) / 4)
    |> Map.put("NextEffectAt", System.monotonic_time(:milliseconds) + Mobile.round_length_in_ms(caster) / 4)
  end
  def process_duration_ability({ability, value}, effects, _target, _caster, _ability, _room) do
    put_in(effects[ability], value)
  end

  def affects_target?(%{} = target, %Ability{} = ability) do
    cond do
      Ability.has_ability?(ability, "AffectsLiving") and Mobile.has_ability?(target, "NonLiving") ->
        false
      Ability.has_ability?(ability, "AffectsAnimals") and !Mobile.has_ability?(target, "Animal") ->
        false
      Ability.has_ability?(ability, "AffectsUndead") and !Mobile.has_ability?(target, "Undead") ->
        false
      Ability.has_ability?(ability, "Poison") and Mobile.has_ability?(target, "PoisonImmunity") ->
        false
      true ->
        true
    end
  end

  def has_ability?(%Ability{} = ability, ability_name) do
    ability.traits
    |> Map.keys
    |> Enum.member?(ability_name)
  end

  def apply_cooldowns(caster, %Ability{} = ability) do
    caster
    |> apply_ability_cooldown(ability)
    |> apply_round_cooldown(ability)
  end

  def apply_ability_cooldown(caster, %Ability{cooldown: nil}), do: caster
  def apply_ability_cooldown(caster, %Ability{cooldown: cooldown, name: name}) do
    Systems.Effect.add(caster, %{"cooldown" => name, "RemoveMessage" => "#{Text.capitalize_first(name)} is ready for use again."}, cooldown)
  end

  def apply_round_cooldown(caster, %Ability{cast_complete: true}), do: caster
  def apply_round_cooldown(caster, %Ability{ignores_round_cooldown?: true}), do: caster
  def apply_round_cooldown(caster, _ability) do
    cooldown = Mobile.round_length_in_ms(caster)
    Systems.Effect.add(caster, %{"cooldown" => :round}, cooldown)
  end

  def caster_cast_message(%Ability{result: :dodged} = ability, %{} = _caster, %{} = target, _mobile) do
    message =
      ability.traits["DodgeUserMessage"]
      |> Text.interpolate(%{"target" => target, "ability" => ability.name})
      |> Text.capitalize_first

    "<p><span class='dark-cyan'>#{message}</span></p>"
  end
  def caster_cast_message(%Ability{result: :resisted} = ability, %{} = _caster, %{} = target, _mobile) do
    message =
      @resist_message.user
      |> Text.interpolate(%{"target" => target, "ability" => ability.name})
      |> Text.capitalize_first

    "<p><span class='dark-cyan'>#{message}</span></p>"
  end
  def caster_cast_message(%Ability{result: :deflected} = _ability, %{} = _caster, %{} = target, _mobile) do
    message =
      @deflect_message.user
      |> Text.interpolate(%{"target" => target})
      |> Text.capitalize_first

    "<p><span class='dark-red'>#{message}</span></p>"
  end
  def caster_cast_message(%Ability{} = ability, %{} = _caster, %{ability_shift: nil} = target, _mobile) do
    message =
      ability.user_message
      |> Text.interpolate(%{"target" => target})
      |> Text.capitalize_first

    color =
      if ability.cast_complete do
        "cyan"
      else
        message_color(ability)
      end

    "<p><span class='#{color}'>#{message}</span></p>"
  end
  def caster_cast_message(%Ability{} = ability, %{} = caster, %{ability_shift: shift} = target, mobile) do
    amount = abs(trunc(shift * Mobile.max_hp_at_level(target, mobile.level)))

    cond do
      amount < 1 and has_ability?(ability, "PhysicalDamage") ->
        ability
        |> Map.put(:result, :deflected)
        |> caster_cast_message(caster, target, mobile)
      amount < 1 and has_ability?(ability, "MagicalDamage") ->
        ability
        |> Map.put(:result, :resisted)
        |> caster_cast_message(caster, target, mobile)
      :else ->
        message =
          case target.ability_special do
            :surprise ->
              String.replace_prefix(ability.user_message, "You ", "You surprise ")
            :crit ->
              String.replace_prefix(ability.user_message, "You ", "You critically ")
            _ ->
              ability.user_message
          end

        message =
          message
          |> Text.interpolate(%{"target" => target, "amount" => amount})
          |> Text.capitalize_first

        color =
          if ability.cast_complete do
            "cyan"
          else
            message_color(ability)
          end

        "<p><span class='#{color}'>#{message}</span></p>"
    end
  end

  def target_cast_message(%Ability{result: :dodged} = ability, %{} = caster, %{} = _target, _mobile) do
    message =
      ability.traits["DodgeTargetMessage"]
      |> Text.interpolate(%{"user" => caster, "ability" => ability.name})
      |> Text.capitalize_first

    "<p><span class='dark-cyan'>#{message}</span></p>"
  end
  def target_cast_message(%Ability{result: :resisted} = ability, %{} = caster, %{} = _target, _mobile) do
    message =
      @resist_message.target
      |> Text.interpolate(%{"user" => caster, "ability" => ability.name})
      |> Text.capitalize_first

    "<p><span class='dark-cyan'>#{message}</span></p>"
  end
  def target_cast_message(%Ability{result: :deflected} = _ability, %{} = caster, %{} = _target, _mobile) do
    message =
      @deflect_message.target
      |> Text.interpolate(%{"user" => caster})
      |> Text.capitalize_first

    "<p><span class='dark-red'>#{message}</span></p>"
  end
  def target_cast_message(%Ability{} = ability, %{} = caster, %{ability_shift: nil} = _target, _mobile) do
    message =
      ability.target_message
      |> Text.interpolate(%{"user" => caster})
      |> Text.capitalize_first

    "<p><span class='#{message_color(ability)}'>#{message}</span></p>"
  end
  def target_cast_message(%Ability{} = ability, %{} = caster, %{ability_shift: _shift} = target, mobile) do
    amount = abs(trunc(target.ability_shift * Mobile.max_hp_at_level(target, mobile.level)))

    cond do
      amount < 1 and has_ability?(ability, "PhysicalDamage") ->
        ability
        |> Map.put(:result, :deflected)
        |> target_cast_message(caster, target, mobile)
      amount < 1 and has_ability?(ability, "MagicalDamage") ->
        ability
        |> Map.put(:result, :resisted)
        |> target_cast_message(caster, target, mobile)
      :else ->
        message =
          case target.ability_special do
            :surprise ->
              String.replace_prefix(ability.target_message, "{{user}} ", "{{user}} surprise ")
            :crit ->
              String.replace_prefix(ability.target_message, "{{user}} ", "{{user}} critically ")
            _ ->
              ability.target_message
          end
        message =
          message
          |> Text.interpolate(%{"user" => caster, "amount" => amount})
          |> Text.capitalize_first

        "<p><span class='#{message_color(ability)}'>#{message}</span></p>"
    end
  end

  def spectator_cast_message(%Ability{result: :dodged} = ability, %{} = caster, %{} = target, _mobile) do
    message =
      ability.traits["DodgeSpectatorMessage"]
      |> Text.interpolate(%{"user" => caster, "target" => target, "ability" => ability.name})
      |> Text.capitalize_first

    "<p><span class='dark-cyan'>#{message}</span></p>"
  end
  def spectator_cast_message(%Ability{result: :resisted} = ability, %{} = caster, %{} = target, _mobile) do
    message =
      @resist_message.spectator
      |> Text.interpolate(%{"user" => caster, "target" => target, "ability" => ability.name})
      |> Text.capitalize_first

    "<p><span class='dark-cyan'>#{message}</span></p>"
  end
  def spectator_cast_message(%Ability{result: :deflected} = _ability, %{} = caster, %{} = target, _mobile) do
    message =
      @deflect_message.spectator
      |> Text.interpolate(%{"user" => caster, "target" => target})
      |> Text.capitalize_first

    "<p><span class='dark-red'>#{message}</span></p>"
  end
  def spectator_cast_message(%Ability{} = ability, %{} = caster, %{ability_shift: nil} = target, _mobile) do
    message =
      ability.spectator_message
      |> Text.interpolate(%{"user" => caster, "target" => target})
      |> Text.capitalize_first

    "<p><span class='#{message_color(ability)}'>#{message}</span></p>"
  end
  def spectator_cast_message(%Ability{} = ability, %{} = caster, %{ability_shift: _shift} = target, mobile) do
    amount = abs(trunc(target.ability_shift * Mobile.max_hp_at_level(target, mobile.level)))

    cond do
      amount < 1 and has_ability?(ability, "PhysicalDamage") ->
        ability
        |> Map.put(:result, :deflected)
        |> spectator_cast_message(caster, target, mobile)
      amount < 1 and has_ability?(ability, "MagicalDamage") ->
        ability
        |> Map.put(:result, :resisted)
        |> spectator_cast_message(caster, target, mobile)
      :else ->
        message =
          case target.ability_special do
            :surprise ->
              String.replace_prefix(ability.spectator_message, "{{user}} ", "{{user}} surprise ")
            :crit ->
              String.replace_prefix(ability.spectator_message, "{{user}} ", "{{user}} critically ")
            _ ->
              ability.spectator_message
          end

        message =
          message
          |> Text.interpolate(%{"user" => caster, "target" => target, "amount" => amount})
          |> Text.capitalize_first

        "<p><span class='#{message_color(ability)}'>#{message}</span></p>"
    end
  end

  def display_cast_message(%Room{} = room, %{} = caster, %{} = target, %Ability{} = ability) do
    room.mobiles
    |> Map.values
    |> Enum.each(fn mobile ->
         cond do
           mobile.ref == caster.ref and not is_nil(ability.user_message) ->
             Mobile.send_scroll(mobile, caster_cast_message(ability, caster, target, mobile))
           mobile.ref == target.ref and not is_nil(ability.target_message) ->
             Mobile.send_scroll(mobile, target_cast_message(ability, caster, target, mobile))
           mobile && not is_nil(ability.spectator_message) ->
             Mobile.send_scroll(mobile, spectator_cast_message(ability, caster, target, mobile))
           true ->
             :noop
         end
       end)
  end

  def display_pre_cast_message(%Room{} = room, %{} = caster, [target_ref | _rest] = targets, %Ability{traits: %{"PreCastMessage" => message}} = ability) do
    target = Room.get_mobile(room, target_ref)

    message =
      message
      |> Text.interpolate(%{"target" => target})
      |> Text.capitalize_first

    Mobile.send_scroll(caster, "<p><span class='#{message_color(ability)}'>#{message}</span></p>")

    display_pre_cast_message(room, caster, targets, update_in(ability.traits, &Map.delete(&1, "PreCastMessage")))
  end
  def display_pre_cast_message(%Room{} = room, %{} = caster, [target_ref | _rest], %Ability{traits: %{"PreCastSpectatorMessage" => message}} = ability) do
    target = Room.get_mobile(room, target_ref)

    message = message
              |> Text.interpolate(%{"user" => caster, "target" => target})
              |> Text.capitalize_first

    Room.send_scroll(room, "<p><span class='#{message_color(ability)}'>#{message}</span></p>", [caster])
  end
  def display_pre_cast_message(_room, _caster, _targets, _ability), do: :noop

  def message_color(%Ability{kind: kind}) when kind in ["attack", "curse"], do: "red"
  def message_color(%Ability{}), do: "blue"

  def can_execute?(%Room{} = room, caster_ref, ability) do
    mobile = Room.get_mobile(room, caster_ref)

    cond do
      cd = on_cooldown?(mobile, ability) ->
        Mobile.send_scroll(mobile, "<p>#{ability.name} is on cooldown: #{time_remaining(mobile, cd)} seconds remaining.</p>")
        false
      cd = on_round_cooldown?(mobile, ability) ->
        Mobile.send_scroll(mobile, "<p>You have already used an ability this round: #{time_remaining(mobile, cd)} seconds remaining.</p>")
        false
      Mobile.confused(mobile, room) ->
        false
      Mobile.silenced(mobile, room) ->
        false
      not_enough_mana?(mobile, ability) ->
        false
      true ->
        true
    end
  end

  def time_remaining(mobile, cd) do
    timer =
      cd
      |> Map.get("timers")
      |> Enum.at(0)

    time = TimerManager.time_remaining(mobile, timer)
    Float.round(time / 1000, 2)
  end

  def on_cooldown?(%{} = _mobile, %Ability{cooldown: nil} = _abilityl), do: false
  def on_cooldown?(%{effects: effects} = _mobile, %Ability{name: name} = _ability) do
    effects
    |> Map.values
    |> Enum.any?(&(&1["cooldown"] == name))
  end

  def on_round_cooldown?(_mobile, %{ignores_round_cooldown?: true}), do: false
  def on_round_cooldown?(mobile, %{}), do: on_round_cooldown?(mobile)
  def on_round_cooldown?(%{effects: effects}) do
    effects
    |> Map.values
    |> Enum.find(&(&1["cooldown"] == :round))
  end

  def get_targets(%Room{} = room, caster_ref, %Ability{targets: "monster or single"}, query) do
    caster = room.mobiles[caster_ref]

    match =
      room.mobiles
      |> Map.values
      |> Enum.reject(& &1.ref in Party.refs(room, caster))
      |> Match.one(:name_contains, query)

    List.wrap(match && match.ref)
  end
  def get_targets(%Room{}, caster_ref, %Ability{targets: "self"}, _query) do
    List.wrap(caster_ref)
  end
  def get_targets(%Room{} = room, _caster_ref, %Ability{targets: "monster"}, query) do
    match =
      room.mobiles
      |> Map.values
      |> Enum.filter(& &1.__struct__ == Monster)
      |> Match.one(:name_contains, query)

    List.wrap(match && match.ref)
  end
  def get_targets(%Room{} = room, caster_ref, %Ability{targets: "full party area"}, _query) do
    room
    |> Room.get_mobile(caster_ref)
    |> Mobile.party_refs(room)
  end
  def get_targets(%Room{} = room, caster_ref, %Ability{targets: "full attack area"}, _query) do
    party =
      room
      |> Room.get_mobile(caster_ref)
      |> Mobile.party_refs(room)

    room.mobiles
    |> Map.keys
    |> Kernel.--(party)
  end
  def get_targets(%Room{}, caster_ref, %Ability{targets: "self or single"}, "") do
    List.wrap(caster_ref)
  end
  def get_targets(%Room{} = room, _caster_ref, %Ability{targets: "self or single"}, query) do
    match =
      room.mobiles
      |> Map.values
      |> Enum.reject(& &1.__struct__ == Monster)
      |> Match.one(:name_contains, query)

    List.wrap(match && match.ref)
  end
  def get_targets(%Room{} = room, caster_ref, %Ability{targets: "single"}, query) do
    match =
      room.mobiles
      |> Map.values
      |> Enum.reject(& &1.__struct__ == Monster || &1.ref == caster_ref)
      |> Match.one(:name_contains, query)

    List.wrap(match && match.ref)
  end

  def not_enough_mana?(%{} = _mobile, %Ability{ignores_round_cooldown?: true}), do: false
  def not_enough_mana?(%{} = mobile, %Ability{} = ability) do
    if !Mobile.enough_mana_for_ability?(mobile, ability) do
      Mobile.send_scroll(mobile, "<p><span class='cyan'>You do not have enough mana to use that ability.</span></p>")
    end
  end

end
