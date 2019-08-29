defmodule ApathyDrive.Ability do
  use ApathyDriveWeb, :model

  alias ApathyDrive.{
    Ability,
    AbilityAttribute,
    AbilityDamageType,
    AbilityTrait,
    Character,
    Companion,
    Directory,
    Enchantment,
    Item,
    ItemInstance,
    Match,
    Mobile,
    Monster,
    Party,
    Repo,
    Room,
    Scripts,
    Stealth,
    Text,
    TimerManager,
    Trait
  }

  require Logger

  schema "abilities" do
    field(:name, :string)
    field(:targets, :string)
    field(:kind, :string)
    field(:mana, :integer, default: 0)
    field(:command, :string)
    field(:description, :string)
    field(:user_message, :string)
    field(:target_message, :string)
    field(:spectator_message, :string)
    field(:duration, :integer, default: 0)
    field(:cooldown, :integer)
    field(:cast_time, :integer)
    field(:energy, :integer, default: 1000)
    field(:difficulty, :integer)
    field(:level, :integer)

    field(:traits, :map, virtual: true, default: %{})
    field(:ignores_round_cooldown?, :boolean, virtual: true, default: false)
    field(:result, :any, virtual: true)
    field(:cast_complete, :boolean, virtual: true, default: false)
    field(:skills, :any, virtual: true, default: [])
    field(:target_list, :any, virtual: true)
    field(:attributes, :map, virtual: true, default: %{})
    field(:max_stacks, :integer, virtual: true, default: 1)
    field(:chance, :integer, virtual: true)
    field(:on_hit?, :boolean, virtual: true, default: false)
    field(:can_crit, :boolean, virtual: true, default: false)
    field(:spell?, :boolean, virtual: true, default: true)
    field(:reaction_energy, :integer, virtual: true)

    has_many(:monsters_abilities, ApathyDrive.MonsterAbility)
    has_many(:monsters, through: [:monsters_abilities, :monster])

    has_many(:death_monsters, ApathyDrive.Monster, foreign_key: :death_ability_id)

    has_many(:abilities_traits, ApathyDrive.AbilityTrait)
    has_many(:trait_records, through: [:abilities_traits, :trait])

    has_many(:abilities_damage_types, ApathyDrive.AbilityDamageType)
    has_many(:damage_types, through: [:abilities_damage_types, :damage_types])

    timestamps()
  end

  @required_fields ~w(name targets kind command description user_message target_message spectator_message)a
  @optional_fields ~w(cast_time cooldown duration mana)a

  @valid_targets [
    "monster or single",
    "self",
    "self or single",
    "monster",
    "full party area",
    "full attack area",
    "single",
    "full area",
    "weapon",
    "armour"
  ]
  @target_required_targets ["monster or single", "monster", "single"]

  @kinds ["heal", "attack", "auto attack", "curse", "utility", "blessing", "passive"]

  @instant_traits [
    "CurePoison",
    "Damage",
    "DispelMagic",
    "Enslave",
    "Freedom",
    "Heal",
    "HealMana",
    "KillSpell",
    "Poison",
    "RemoveSpells",
    "Script",
    "Summon",
    "Teleport"
  ]

  @duration_traits [
    "AC",
    "AC%",
    "Accuracy",
    "Agility",
    "Charm",
    "Blind",
    "Charm",
    "Confusion",
    "ConfusionMessage",
    "ConfusionSpectatorMessage",
    "Crits",
    "Damage",
    "DamageShield",
    "DamageShieldUserMessage",
    "DamageShieldTargetMessage",
    "DamageShieldSpectatorMessage",
    "DarkVision",
    "Dodge",
    "Encumbrance",
    "EndCast",
    "EndCast%",
    "Fear",
    "Heal",
    "Health",
    "HPRegen",
    "Intellect",
    "Light",
    "LightVision",
    "MR",
    "MR%",
    "ManaRegen",
    "MaxHP",
    "MaxMana",
    "ModifyDamage",
    "Perception",
    "Picklocks",
    "PoisonImmunity",
    "RemoveMessage",
    "ResistCold",
    "ResistFire",
    "ResistLightning",
    "ResistStone",
    "Root",
    "SeeHidden",
    "Shadowform",
    "Silence",
    "Speed",
    "Spellcasting",
    "StatusMessage",
    "Stealth",
    "Strength",
    "Tracking",
    "Willpower"
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
    |> cast(params, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
  end

  def ac_for_mitigation_at_level(mitigation_percent, level) do
    percent = mitigation_percent / 100
    trunc(Float.round(-(50 * level * percent / (percent - 1))))
  end

  def data_for_admin_index do
    __MODULE__
    |> select(
      [dt],
      map(
        dt,
        ~w(id name targets kind mana command description user_message target_message spectator_message duration cooldown cast_time)a
      )
    )
  end

  def total_damage(%Ability{traits: %{"Damage" => damage}}) do
    damage
    |> Enum.map(& &1.damage)
    |> Enum.sum()
  end

  def total_damage(%Ability{}), do: 0

  def valid_targets, do: @valid_targets
  def kinds, do: @kinds

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

  def find(name) when is_binary(name) do
    ability = ApathyDrive.Repo.get_by(__MODULE__, name: name)

    if ability do
      load(ability)
    end
  end

  def find(id) do
    ability = ApathyDrive.Repo.get(__MODULE__, id)

    if ability do
      load(ability)
    end
  end

  def load(ability) do
    ability = put_in(ability.traits, AbilityTrait.load_traits(ability.id))

    case AbilityDamageType.load_damage(ability.id) do
      [] ->
        ability

      damage ->
        update_in(ability.traits, &Map.put(&1, "Damage", damage))
    end
  end

  def match_by_name(name, all \\ false) do
    abilities =
      if all do
        learnable_abilities()
      else
        __MODULE__
        |> where([ability], not is_nil(ability.name) and ability.name != "")
        |> distinct(true)
        |> ApathyDrive.Repo.all()
      end
      |> Enum.map(fn ability ->
        attributes = AbilityAttribute.load_attributes(ability.id)
        Map.put(ability, :attributes, attributes)
      end)

    Match.all(abilities, :keyword_starts_with, name)
  end

  def learnable_abilities do
    class_ability_ids =
      ApathyDrive.ClassAbility
      |> select([:ability_id])
      |> distinct(true)
      |> preload(:ability)
      |> Repo.all()

    learn_id =
      ApathyDrive.ItemAbilityType
      |> select([:id])
      |> where(name: "Learn")
      |> Repo.one!()
      |> Map.get(:id)

    scroll_ability_ids =
      ApathyDrive.ItemAbility
      |> select([:ability_id])
      |> where(type_id: ^learn_id)
      |> distinct(true)
      |> preload(:ability)
      |> Repo.all()

    skill_ability_ids =
      ApathyDrive.SkillAbility
      |> select([:ability_id])
      |> distinct(true)
      |> preload(:ability)
      |> Repo.all()

    (class_ability_ids ++ scroll_ability_ids ++ skill_ability_ids)
    |> Enum.map(& &1.ability)
    |> Enum.uniq()
    |> Enum.reject(&(is_nil(&1.name) or &1.name == ""))
  end

  def heal_abilities(%{abilities: abilities} = mobile) do
    abilities
    |> Map.values()
    |> List.flatten()
    |> Enum.filter(&(&1.kind == "heal"))
    |> useable(mobile)
  end

  def drain_abilities(%{abilities: abilities} = mobile, %{} = target) do
    abilities
    |> Map.values()
    |> List.flatten()
    |> Enum.filter(fn ability ->
      Map.has_key?(ability.traits, "Damage") and
        Enum.any?(ability.traits["Damage"], &(&1.kind == "drain"))
    end)
    |> Enum.filter(fn ability ->
      Ability.affects_target?(target, ability)
    end)
    |> useable(mobile)
  end

  def bless_abilities(%{abilities: abilities} = mobile, %{} = target) do
    abilities
    |> Map.values()
    |> List.flatten()
    |> Enum.filter(&(&1.kind == "blessing"))
    |> Enum.reject(fn ability ->
      removes_blessing?(target, ability)
    end)
    |> useable(mobile)
  end

  def curse_abilities(%{abilities: abilities} = mobile, %{} = target) do
    abilities
    |> Map.values()
    |> List.flatten()
    |> Enum.filter(&(&1.kind == "curse"))
    |> Enum.reject(fn ability ->
      Ability.removes_blessing?(target, ability)
    end)
    |> useable(mobile)
  end

  def attack_abilities(%{abilities: abilities} = mobile, %{} = target) do
    abilities
    |> Map.values()
    |> List.flatten()
    |> Enum.filter(&(&1.kind == "attack"))
    |> Enum.filter(fn ability ->
      Ability.affects_target?(target, ability)
    end)
    |> useable(mobile)
  end

  def useable(abilities, %{} = mobile) do
    abilities
    |> List.flatten()
    |> Enum.filter(fn ability ->
      Mobile.enough_mana_for_ability?(mobile, ability) && !Ability.on_cooldown?(mobile, ability)
    end)
  end

  def removes_blessing?(%{} = mobile, %{} = ability) do
    abilities = ability.traits["RemoveSpells"] || []

    Systems.Effect.max_stacks?(mobile, ability) or
      Enum.any?(abilities, fn ability_id ->
        Systems.Effect.stack_count(mobile, ability_id) > 0
      end)
  end

  def removes_blessing?(mobile, ability) do
    Systems.Effect.max_stacks?(mobile, ability)
  end

  def select_ability(_character, [ability]), do: ability

  def select_ability(character, abilities) do
    Enum.sort_by(abilities, fn ability ->
      cond do
        ability.cooldown > 0 and !Ability.on_cooldown?(character, ability) ->
          -1

        is_nil(ability.cooldown) ->
          0

        cd = on_cooldown?(character, ability) ->
          time_remaining(character, cd)
      end
    end)
    |> List.first()
  end

  def execute(%Room{} = room, caster_ref, %Ability{targets: targets}, "")
      when targets in @target_required_targets do
    room
    |> Room.get_mobile(caster_ref)
    |> Mobile.send_scroll(
      "<p><span class='red'>You must specify a target for that ability.</span></p>"
    )

    room
  end

  def execute(%Room{} = room, caster_ref, %Ability{} = ability, query) when is_binary(query) do
    case get_targets(room, caster_ref, ability, query) do
      [] ->
        case query do
          "" ->
            if ability.targets in ["self", "self or single"] do
              execute(room, caster_ref, ability, List.wrap(caster_ref))
            else
              room
              |> Room.get_mobile(caster_ref)
              |> Mobile.send_scroll("<p>Your ability would affect no one.</p>")

              room
            end

          _ ->
            room
            |> Room.get_mobile(caster_ref)
            |> Mobile.send_scroll(
              "<p><span class='cyan'>Can't find #{query} here! Your spell fails.</span></p>"
            )

            room
        end

      :too_many_matches ->
        room

      targets ->
        execute(room, caster_ref, ability, targets)
    end
  end

  def execute(%Room{} = room, caster_ref, nil, %Item{} = item) do
    Room.update_mobile(room, caster_ref, fn
      caster ->
        room =
          Room.update_mobile(room, caster.ref, fn caster ->
            caster =
              caster
              |> Stealth.reveal()

            Mobile.update_prompt(caster)

            caster =
              if lt = Enum.find(TimerManager.timers(caster), &match?({:longterm, _}, &1)) do
                Mobile.send_scroll(
                  caster,
                  "<p><span class='cyan'>You interrupt your work.</span></p>"
                )

                TimerManager.cancel(caster, lt)
              else
                caster
              end

            Enchantment
            |> Ecto.Query.where(
              [e],
              e.items_instances_id == ^item.instance_id and is_nil(e.ability_id)
            )
            |> Repo.all()
            |> case do
              [%Enchantment{finished: false} = enchantment] ->
                enchantment = Repo.preload(enchantment, :items_instances)
                time = Enchantment.next_tick_time(enchantment)

                Mobile.send_scroll(
                  caster,
                  "<p><span class='cyan'>You continue your work.</span></p>"
                )

                Mobile.send_scroll(
                  caster,
                  "<p><span class='dark-green'>Time Left:</span> <span class='dark-cyan'>#{
                    Enchantment.time_left(enchantment) |> Enchantment.formatted_time_left()
                  }</span></p>"
                )

                TimerManager.send_after(
                  caster,
                  {{:longterm, item.instance_id}, :timer.seconds(time),
                   {:lt_tick, time, caster_ref, enchantment}}
                )

              [] ->
                enchantment =
                  %Enchantment{items_instances_id: item.instance_id, ability_id: nil}
                  |> Repo.insert!()
                  |> Repo.preload(:items_instances)

                time = Enchantment.next_tick_time(enchantment)
                Mobile.send_scroll(caster, "<p><span class='cyan'>You begin work.</span></p>")

                Mobile.send_scroll(
                  caster,
                  "<p><span class='dark-green'>Time Left:</span> <span class='dark-cyan'>#{
                    Enchantment.time_left(enchantment) |> Enchantment.formatted_time_left()
                  }</span></p>"
                )

                TimerManager.send_after(
                  caster,
                  {{:longterm, item.instance_id}, :timer.seconds(time),
                   {:lt_tick, time, caster_ref, enchantment}}
                )
            end
          end)

        Room.update_moblist(room)

        room
    end)
  end

  def execute(%Room{} = room, caster_ref, %Ability{kind: "long-term"} = ability, %Item{} = item) do
    traits =
      ability.traits
      |> Map.update("RequireItems", [item.instance_id], &[item.instance_id | &1])
      |> Map.put(
        "TickMessage",
        "<p><span class='dark-cyan'>You continue enchanting the #{item.name}.</span></p>"
      )

    ability = Map.put(ability, :traits, traits)

    Room.update_mobile(room, caster_ref, fn caster ->
      cond do
        mobile = not_enough_energy(caster, Map.put(ability, :target_list, item)) ->
          mobile

        casting_failed?(caster, ability) ->
          casting_failed(room, caster_ref, ability)

        quality_too_high?(ability, item) ->
          Mobile.send_scroll(
            caster,
            "<p>#{Item.colored_name(item)} cannot be enchanted to a quality level higher than #{
              Item.max_quality(item)
            }.<p>"
          )

          caster

        can_execute?(room, caster, ability) ->
          display_pre_cast_message(room, caster, item, ability)

          room =
            Room.update_mobile(room, caster.ref, fn caster ->
              caster =
                caster
                |> apply_cooldowns(ability)
                |> Mobile.subtract_mana(ability)
                |> Mobile.subtract_energy(ability)
                |> Stealth.reveal()

              Mobile.update_prompt(caster)

              caster =
                if lt = Enum.find(TimerManager.timers(caster), &match?({:longterm, _}, &1)) do
                  Mobile.send_scroll(
                    caster,
                    "<p><span class='cyan'>You interrupt your work.</span></p>"
                  )

                  TimerManager.cancel(caster, lt)
                else
                  caster
                end

              case Repo.get_by(
                     Enchantment,
                     items_instances_id: item.instance_id,
                     ability_id: ability.id,
                     finished: false
                   ) do
                %Enchantment{finished: false} = enchantment ->
                  enchantment = Map.put(enchantment, :ability, ability)
                  time = Enchantment.next_tick_time(enchantment)

                  Mobile.send_scroll(
                    caster,
                    "<p><span class='cyan'>You continue your work.</span></p>"
                  )

                  Mobile.send_scroll(
                    caster,
                    "<p><span class='dark-green'>Time Left:</span> <span class='dark-cyan'>#{
                      Enchantment.time_left(enchantment) |> Enchantment.formatted_time_left()
                    }</span></p>"
                  )

                  TimerManager.send_after(
                    caster,
                    {{:longterm, item.instance_id}, :timer.seconds(time),
                     {:lt_tick, time, caster_ref, enchantment}}
                  )

                nil ->
                  start_enchantment(caster, item, ability)
              end
            end)

          Room.update_moblist(room)

          Room.update_energy_bar(room, caster.ref)
          Room.update_hp_bar(room, caster.ref)
          Room.update_mana_bar(room, caster.ref)

          room

        :else ->
          room
      end
    end)
  end

  def execute(%Room{} = room, caster_ref, %Ability{} = ability, targets) when is_list(targets) do
    Room.update_mobile(room, caster_ref, fn caster ->
      cond do
        mobile = not_enough_energy(caster, Map.put(ability, :target_list, targets)) ->
          mobile

        casting_failed?(caster, ability) ->
          casting_failed(room, caster_ref, ability)

        can_execute?(room, caster, ability) ->
          display_pre_cast_message(room, caster, targets, ability)

          ability = crit(caster, ability)

          room =
            Enum.reduce(targets, room, fn target_ref, updated_room ->
              Room.update_mobile(updated_room, target_ref, fn target ->
                caster = updated_room.mobiles[caster.ref]

                if affects_target?(target, ability) do
                  updated_room = apply_ability(updated_room, caster, target, ability)

                  target = updated_room.mobiles[target_ref]

                  if target do
                    target =
                      if ability.kind in ["attack", "curse"] do
                        Stealth.reveal(target)
                      else
                        target
                      end

                    max_hp = Mobile.max_hp_at_level(target, target.level)
                    hp = trunc(max_hp * target.hp)

                    if hp < 1 do
                      Mobile.die(target, updated_room)
                    else
                      put_in(updated_room.mobiles[target.ref], target)
                    end
                  else
                    updated_room
                  end
                else
                  message =
                    "#{target.name} is not affected by that ability." |> Text.capitalize_first()

                  Mobile.send_scroll(caster, "<p><span class='cyan'>#{message}</span></p>")
                  target
                end
              end)
            end)

          Room.update_moblist(room)

          room =
            Room.update_mobile(room, caster.ref, fn caster ->
              caster =
                caster
                |> apply_cooldowns(ability)
                |> Mobile.subtract_mana(ability)
                |> Mobile.subtract_energy(ability)

              Mobile.update_prompt(caster)

              if ability.kind in ["attack", "curse"] and !(caster.ref in targets) do
                [target_ref | _] = targets

                if Map.has_key?(caster, :attack_target) do
                  if is_nil(caster.attack_target) do
                    caster
                    |> Map.put(:attack_target, target_ref)
                    |> Mobile.send_scroll(
                      "<p><span class='dark-yellow'>*Combat Engaged*</span></p>"
                    )
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

          Room.update_energy_bar(room, caster.ref)
          Room.update_hp_bar(room, caster.ref)
          Room.update_mana_bar(room, caster.ref)

          room = Room.update_mobile(room, caster_ref, &Stealth.reveal(&1))

          Room.update_moblist(room)

          room =
            if instance_id = ability.traits["DestroyItem"] do
              Room.update_mobile(room, caster_ref, fn caster ->
                scroll =
                  (caster.inventory ++ caster.equipment)
                  |> Enum.find(&(&1.instance_id == instance_id))

                Mobile.send_scroll(
                  caster,
                  "<p>As you read the #{scroll.name} it crumbles to dust.</p>"
                )

                ItemInstance
                |> Repo.get!(instance_id)
                |> Repo.delete!()

                caster
                |> Character.load_abilities()
                |> Character.load_items()
              end)
            else
              room
            end

          if (on_hit = ability.traits["OnHit"]) && is_nil(Process.get(:ability_result)) &&
               :rand.uniform(100) <= ability.traits["OnHit%"] do
            Process.delete(:ability_result)
            execute(room, caster_ref, Enum.random(on_hit), targets)
          else
            Process.delete(:ability_result)
            room
          end

        :else ->
          room
      end
    end)
  end

  def start_enchantment(caster, item, ability) do
    enchantment =
      %Enchantment{items_instances_id: item.instance_id, ability_id: ability.id}
      |> Repo.insert!()
      |> Map.put(:ability, ability)

    time = Enchantment.next_tick_time(enchantment)
    Mobile.send_scroll(caster, "<p><span class='cyan'>You begin work.</span></p>")

    Mobile.send_scroll(
      caster,
      "<p><span class='dark-green'>Time Left:</span> <span class='dark-cyan'>#{
        Enchantment.time_left(enchantment) |> Enchantment.formatted_time_left()
      }</span></p>"
    )

    TimerManager.send_after(
      caster,
      {{:longterm, item.instance_id}, :timer.seconds(time),
       {:lt_tick, time, caster.ref, enchantment}}
    )
  end

  def not_enough_energy(%{energy: energy} = caster, %{energy: req_energy} = ability) do
    if req_energy > energy && !ability.on_hit? do
      if caster.casting do
        Mobile.send_scroll(
          caster,
          "<p><span class='dark-red'>You interrupt your other spell.</span></p>"
        )
      end

      if ability.spell? do
        Mobile.send_scroll(caster, "<p><span class='cyan'>You begin your casting.</span></p>")
      else
        Mobile.send_scroll(caster, "<p><span class='cyan'>You move into position...</span></p>")
      end

      Map.put(caster, :casting, ability)
    end
  end

  def duration(%Ability{duration: duration} = _ability, %{} = _caster, %{} = _target, _room) do
    duration
  end

  def dodged?(%{} = caster, %{} = target, room) do
    accuracy = Mobile.accuracy_at_level(caster, caster.level, room)

    dodge = Mobile.dodge_at_level(target, target.level, room)

    modifier = Mobile.ability_value(target, "Dodge")

    difference = dodge - accuracy

    chance =
      if difference > 0 do
        30 + modifier + difference * 0.3
      else
        30 + modifier + difference * 0.7
      end

    :rand.uniform(100) < chance
  end

  def blocked?(%{} = caster, %Character{} = target, room) do
    if Character.shield(target) do
      accuracy = Mobile.accuracy_at_level(caster, caster.level, room)

      block = Mobile.block_at_level(target, target.level)

      modifier = Mobile.ability_value(target, "Block")

      difference = block - accuracy

      chance =
        if difference > 0 do
          30 + modifier + difference * 0.3
        else
          30 + modifier + difference * 0.7
        end

      :rand.uniform(100) < chance
    else
      false
    end
  end

  def blocked?(%{} = _caster, %{} = target, _room) do
    :rand.uniform(100) < Mobile.ability_value(target, "Block")
  end

  def parried?(%{} = caster, %Character{} = target, room) do
    if Character.weapon(target) do
      accuracy = Mobile.accuracy_at_level(caster, caster.level, room)

      dodge = Mobile.parry_at_level(target, target.level)

      modifier = Mobile.ability_value(target, "Parry")

      difference = dodge - accuracy

      chance =
        if difference > 0 do
          30 + modifier + difference * 0.3
        else
          30 + modifier + difference * 0.7
        end

      :rand.uniform(100) < chance
    else
      false
    end
  end

  def parried?(%{} = _caster, %{} = target, _room) do
    :rand.uniform(100) < Mobile.ability_value(target, "Parry")
  end

  def apply_ability(
        %Room{} = room,
        %{} = caster,
        %{} = target,
        %Ability{traits: %{"Dodgeable" => true}} = ability
      ) do
    cond do
      dodged?(caster, target, room) ->
        room = add_evil_points(room, ability, caster, target)
        caster = room.mobiles[caster.ref]
        target = room.mobiles[target.ref]
        Process.put(:ability_result, :dodged)
        display_cast_message(room, caster, target, Map.put(ability, :result, :dodged))

        target =
          target
          |> aggro_target(ability, caster)
          |> Character.add_attribute_experience(%{
            agility: 0.9,
            charm: 0.1
          })

        put_in(room.mobiles[target.ref], target)

      blocked?(caster, target, room) ->
        room = add_evil_points(room, ability, caster, target)
        caster = room.mobiles[caster.ref]
        target = room.mobiles[target.ref]
        Process.put(:ability_result, :blocked)
        display_cast_message(room, caster, target, Map.put(ability, :result, :blocked))

        target =
          target
          |> aggro_target(ability, caster)
          |> Character.add_attribute_experience(%{
            strength: 0.7,
            agility: 0.2,
            charm: 0.1
          })

        put_in(room.mobiles[target.ref], target)
        |> trigger_damage_shields(caster.ref, target.ref, ability)

      parried?(caster, target, room) ->
        room = add_evil_points(room, ability, caster, target)
        caster = room.mobiles[caster.ref]
        target = room.mobiles[target.ref]
        Process.put(:ability_result, :parried)
        display_cast_message(room, caster, target, Map.put(ability, :result, :parried))

        target =
          target
          |> aggro_target(ability, caster)
          |> Character.add_attribute_experience(%{
            strength: 0.2,
            agility: 0.7,
            charm: 0.1
          })

        put_in(room.mobiles[target.ref], target)
        |> trigger_damage_shields(caster.ref, target.ref, ability)

      true ->
        apply_ability(
          room,
          caster,
          target,
          update_in(ability.traits, &Map.delete(&1, "Dodgeable"))
        )
    end
  end

  def apply_ability(
        %Room{} = room,
        %Character{} = caster,
        %{} = target,
        %Ability{traits: %{"Enslave" => _}} = ability
      ) do
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
    room = add_evil_points(room, ability, caster, target)
    caster = room.mobiles[caster.ref]
    target = room.mobiles[target.ref]

    {caster, target} =
      target
      |> apply_instant_traits(ability, caster, room)

    target = aggro_target(target, ability, caster)

    room = put_in(room.mobiles[caster.ref], caster)
    room = put_in(room.mobiles[target.ref], target)

    duration = duration(ability, caster, target, room)

    if ability.kind == "curse" and duration < 1 do
      Process.put(:ability_result, :resisted)
      display_cast_message(room, caster, target, Map.put(ability, :result, :resisted))

      target =
        target
        |> Map.put(:ability_shift, nil)
        |> Map.put(:ability_special, nil)
        |> Mobile.update_prompt()

      room
      |> put_in([:mobiles, target.ref], target)
    else
      display_cast_message(room, caster, target, ability)

      room
      |> trigger_damage_shields(caster.ref, target.ref, ability)
      |> finish_ability(caster.ref, target.ref, ability, target.ability_shift)
    end
  end

  def add_evil_points(room, %Ability{kind: kind} = ability, %Character{} = caster, target)
      when kind in ["attack", "auto attack", "curse"] do
    evil_points = Mobile.evil_points(target, caster)

    if evil_points > 0 do
      initial_caster_legal_status = Character.legal_status(caster)

      Logger.info("increasing #{caster.name}'s evil points by #{evil_points}")

      caster =
        caster
        |> Ecto.Changeset.change(%{
          evil_points: min(300, caster.evil_points + evil_points),
          last_evil_action_at: DateTime.utc_now()
        })
        |> Repo.update!()

      Directory.add_character(%{
        name: caster.name,
        evil_points: caster.evil_points,
        room: caster.room_id,
        ref: caster.ref,
        title: caster.title
      })

      caster_legal_status = Character.legal_status(caster)

      Mobile.send_scroll(
        caster,
        "<p><span class='dark-grey'>A dark cloud passes over you</span></p>"
      )

      if caster_legal_status != initial_caster_legal_status do
        color = ApathyDrive.Commands.Who.color(caster_legal_status)

        status = "<span class='#{color}'>#{caster_legal_status}</span>"

        Mobile.send_scroll(
          caster,
          "<p>Your legal status has changed to #{status}.</p>"
        )

        Room.send_scroll(
          room,
          "<p>#{Mobile.colored_name(caster)}'s legal status has changed to #{status}.",
          [caster]
        )
      end

      retaliate(room, ability, caster, target)
    else
      retaliate(room, ability, caster, target)
    end
  end

  def add_evil_points(room, ability, caster, target) do
    retaliate(room, ability, caster, target)
  end

  def retaliate?(caster, target) do
    targets =
      caster.effects
      |> Map.values()
      |> Enum.filter(&Map.has_key?(&1, "Aggro"))
      |> Enum.map(&Map.get(&1, "Aggro"))

    target.ref in targets
  end

  def retaliate(room, %Ability{kind: kind}, caster, target)
      when kind in ["attack", "auto attack", "curse"] do
    caster =
      Systems.Effect.add(
        caster,
        %{
          "Retaliation" => target.ref,
          "stack_key" => {:retaliation, target.ref},
          "stack_count" => 1
        },
        :timer.minutes(20)
      )

    target =
      Systems.Effect.add(
        target,
        %{
          "Retaliation" => caster.ref,
          "stack_key" => {:retaliation, caster.ref},
          "stack_count" => 1
        },
        :timer.minutes(20)
      )

    room = put_in(room.mobiles[caster.ref], caster)
    put_in(room.mobiles[target.ref], target)
  end

  def retaliate(room, _ability, _caster, _target), do: room

  def finish_ability(room, caster_ref, target_ref, ability, ability_shift) do
    room =
      Room.update_mobile(room, target_ref, fn target ->
        caster = room.mobiles[caster_ref]

        duration = duration(ability, caster, target, room)

        target =
          if ability_shift do
            Mobile.shift_hp(target, ability_shift)
          else
            target
          end

        target
        |> Map.put(:ability_shift, nil)
        |> Map.put(:ability_special, nil)
        |> apply_duration_traits(ability, caster, duration)
        |> Mobile.update_prompt()
      end)

    room =
      if script = ability.traits["Script"] do
        Room.update_mobile(room, caster_ref, fn caster ->
          Module.safe_concat([Scripts, Macro.camelize(script)]).execute(room, caster.ref)
        end)
      else
        room
      end

    Room.update_hp_bar(room, target_ref)
    Room.update_hp_bar(room, caster_ref)
    Room.update_mana_bar(room, caster_ref)
    Room.update_mana_bar(room, target_ref)

    room
  end

  def trigger_damage_shields(%Room{} = room, caster_ref, target_ref, _ability)
      when target_ref == caster_ref,
      do: room

  def trigger_damage_shields(%Room{} = room, caster_ref, target_ref, ability) do
    if (target = room.mobiles[target_ref]) && "Damage" in Map.keys(ability.traits) do
      target
      |> Map.get(:effects)
      |> Map.values()
      |> Enum.filter(&Map.has_key?(&1, "DamageShield"))
      |> Enum.reduce(room, fn %{"DamageShield" => shield}, updated_room ->
        reaction = %Ability{
          kind: "attack",
          mana: 0,
          energy: 0,
          reaction_energy: Enum.random(100..300),
          user_message: shield["UserMessage"],
          target_message: shield["TargetMessage"],
          spectator_message: shield["SpectatorMessage"],
          traits: %{
            "Damage" => shield["Damage"]
          }
        }

        apply_ability(updated_room, room.mobiles[target_ref], room.mobiles[caster_ref], reaction)
      end)
    else
      room
    end
  end

  def aggro_target(
        %Character{ref: target_ref} = target,
        %Ability{kind: kind},
        %{ref: caster_ref} = _caster
      )
      when kind in ["attack", "curse"] and target_ref != caster_ref do
    # players don't automatically fight back
    target
  end

  def aggro_target(%{ref: target_ref} = target, %Ability{kind: kind}, %{ref: caster_ref} = caster)
      when kind in ["attack", "curse"] and target_ref != caster_ref do
    ApathyDrive.Aggression.attack_target(target, caster)
  end

  def aggro_target(%{} = target, %Ability{}, %{} = _caster), do: target

  def apply_instant_traits(%{} = target, %Ability{} = ability, %{} = caster, room) do
    ability.traits
    |> Map.take(@instant_traits)
    |> Enum.reduce({caster, target}, fn
      {"Damage", _} = trait, {updated_caster, updated_target} ->
        if "DamageShield" in Map.keys(ability.traits) do
          {updated_caster, updated_target}
        else
          apply_instant_trait(trait, updated_target, ability, updated_caster, room)
        end

      trait, {updated_caster, updated_target} ->
        apply_instant_trait(trait, updated_target, ability, updated_caster, room)
    end)
  end

  def apply_instant_trait({"RemoveSpells", ability_ids}, %{} = target, _ability, caster, _room) do
    target =
      Enum.reduce(ability_ids, target, fn ability_id, updated_target ->
        Systems.Effect.remove_oldest_stack(updated_target, ability_id)
      end)

    {caster, target}
  end

  def apply_instant_trait({"Heal", value}, %{} = target, _ability, caster, _room)
      when is_float(value) do
    {caster, Map.put(target, :ability_shift, value)}
  end

  def apply_instant_trait({"Heal", value}, %{} = target, _ability, caster, _room) do
    healing = Enum.random(value["min"]..value["max"])

    percentage_healed = healing / Mobile.max_hp_at_level(target, target.level)

    {caster, Map.put(target, :ability_shift, percentage_healed)}
  end

  def apply_instant_trait({"Damage", value}, %{} = target, _ability, caster, _room)
      when is_float(value) do
    {caster, Map.put(target, :ability_shift, -value)}
  end

  def apply_instant_trait({"Damage", damages}, %{} = target, ability, caster, _room) do
    round_percent = (ability.reaction_energy || ability.energy) / caster.max_energy

    target =
      target
      |> Map.put(:ability_shift, 0)

    level =
      target.level
      |> max(caster.level)
      |> min(50)

    {caster, damage_percent} =
      Enum.reduce(damages, {caster, 0}, fn
        %{kind: "physical", min: min, max: max, damage_type: type}, {caster, damage_percent} ->
          min = trunc(min)
          max = trunc(max)

          ability_damage = Enum.random(min..max)

          bonus_damage = Mobile.ability_value(caster, "ModifyDamage") * round_percent

          resist = Mobile.physical_resistance_at_level(target, target.level)

          resist = resist - Mobile.physical_penetration_at_level(caster, caster.level)

          resist_percent = 1 - resist / (level * 50 + resist)

          damage = (ability_damage + bonus_damage) * resist_percent

          modifier = Mobile.ability_value(target, "Resist#{type}")

          damage = damage * (1 - modifier / 100)

          percent = damage / Mobile.max_hp_at_level(target, target.level)

          {caster, damage_percent + percent}

        %{kind: "physical", damage: dmg, damage_type: type}, {caster, damage_percent} ->
          resist = Mobile.physical_resistance_at_level(target, target.level)

          resist_percent = 1 - resist / (level * 50 + resist)

          damage = dmg * resist_percent

          modifier = Mobile.ability_value(target, "Resist#{type}")

          damage = damage * (1 - modifier / 100)

          percent = damage / Mobile.max_hp_at_level(target, target.level)

          {caster, damage_percent + percent}

        %{kind: "magical", min: min, max: max, damage_type: type}, {caster, damage_percent} ->
          min = trunc(min)
          max = trunc(max)

          ability_damage = Enum.random(min..max)

          bonus_damage = Mobile.ability_value(caster, "ModifyDamage") * round_percent

          resist = Mobile.magical_resistance_at_level(target, target.level)

          resist = resist - Mobile.magical_penetration_at_level(caster, caster.level)

          resist_percent = 1 - resist / (level * 50 + resist)

          damage = (ability_damage + bonus_damage) * resist_percent

          modifier = Mobile.ability_value(target, "Resist#{type}")

          damage = damage * (1 - modifier / 100)

          max_hp = Mobile.max_hp_at_level(target, target.level)

          percent = damage / max_hp

          {caster, damage_percent + percent}

        %{kind: "magical", damage: damage, damage_type: type}, {caster, damage_percent} ->
          resist = Mobile.magical_resistance_at_level(target, target.level)

          resist_percent = 1 - resist / (level * 50 + resist)

          damage = damage * resist_percent

          modifier = Mobile.ability_value(target, "Resist#{type}")

          damage = damage * (1 - modifier / 100)

          percent = damage / Mobile.max_hp_at_level(target, target.level)

          {caster, damage_percent + percent}

        %{kind: "drain", min: min, max: max, damage_type: type}, {caster, damage_percent} ->
          min = trunc(min)
          max = trunc(max)

          ability_damage = Enum.random(min..max)

          bonus_damage = Mobile.ability_value(caster, "ModifyDamage") * round_percent

          resist = Mobile.magical_resistance_at_level(target, target.level)

          resist = resist - Mobile.magical_penetration_at_level(caster, caster.level)

          resist_percent = 1 - resist / (level * 50 + resist)

          damage = (ability_damage + bonus_damage) * resist_percent

          modifier = Mobile.ability_value(target, "Resist#{type}")

          damage = damage * (1 - modifier / 100)

          percent = damage / Mobile.max_hp_at_level(target, target.level)

          heal_percent = damage / Mobile.max_hp_at_level(caster, caster.level)

          caster = Mobile.shift_hp(caster, heal_percent)

          Mobile.update_prompt(caster)

          {caster, damage_percent + percent}
      end)

    damage_percent =
      if match?(%Character{}, target) and target.level < 5 and damage_percent > 0.2 do
        Enum.random(10..20) / 100
      else
        damage_percent
      end

    target =
      target
      |> Map.put(:ability_special, :normal)
      |> Map.update(:ability_shift, 0, &(&1 - damage_percent))

    target_attribute =
      if ability.mana > 0 do
        :willpower
      else
        :strength
      end

    target =
      Character.add_attribute_experience(target, %{
        target_attribute => 0.2,
        :health => 0.8
      })

    {caster, target}
  end

  # just to silence the Not Implemented, handled elsewhere
  def apply_instant_trait({"Script", _id}, %{} = target, _ability, caster, _room) do
    {caster, target}
  end

  def apply_instant_trait({ability_name, _value}, %{} = target, _ability, caster, _room) do
    Mobile.send_scroll(caster, "<p><span class='red'>Not Implemented: #{ability_name}")
    {caster, target}
  end

  def crit(caster, %Ability{can_crit: true, traits: %{"Damage" => _}} = ability) do
    crit_chance = Mobile.crits_at_level(caster, caster.level)

    crit_message = fn message ->
      message
      |> String.split(" ")
      |> List.insert_at(1, "critically")
      |> Enum.join(" ")
    end

    if :rand.uniform(100) < crit_chance do
      ability
      |> update_in([Access.key!(:traits), "Damage"], fn damages ->
        Enum.map(damages, fn damage ->
          damage
          |> Map.put(:max, damage.max * 2)
          |> Map.put(:min, damage.min * 2)
        end)
      end)
      |> Map.update!(:spectator_message, &crit_message.(&1))
      |> Map.update!(:target_message, &crit_message.(&1))
      |> Map.update!(:user_message, &crit_message.(&1))
    else
      ability
    end
  end

  def crit(_caster, ability), do: ability

  def calculate_healing(damage, modifier) do
    damage * (modifier / 100) * (Enum.random(95..105) / 100)
  end

  def apply_item_enchantment(%Item{} = item, %Ability{} = ability) do
    effects =
      ability.traits
      |> Map.take(@duration_traits)
      |> Map.put("stack_key", ability.id)
      |> Map.put("stack_count", 1)
      |> Map.put("effect_ref", make_ref())

    Systems.Effect.add(item, effects)
  end

  def apply_duration_traits(%{} = target, %Ability{} = ability, %{} = caster, duration) do
    effects =
      ability.traits
      |> Map.take(@duration_traits)
      |> Map.put("stack_key", ability.id)
      |> Map.put("stack_count", ability.traits["StackCount"] || 1)
      |> process_duration_traits(target, caster)
      |> Map.put("effect_ref", make_ref())

    if message = effects["StatusMessage"] do
      Mobile.send_scroll(
        target,
        "<p><span class='#{message_color(ability)}'>#{message}</span></p>"
      )
    end

    if has_passive_ability?(target, ability.id) do
      target
    else
      target
      |> Systems.Effect.add(effects, :timer.seconds(duration))
    end
  end

  def has_passive_ability?(target, ability_id) do
    target.effects
    |> Map.values()
    |> Enum.filter(&Map.has_key?(&1, "Passive"))
    |> Enum.any?(fn
      %{"Passive" => abilities} when is_list(abilities) ->
        has_ability? = ability_id in Enum.map(abilities, & &1.id)

        has_conflicting_ability? =
          Enum.any?(abilities, fn ability ->
            case ability.traits["RemoveSpells"] do
              nil ->
                false

              list ->
                ability_id in list
            end
          end)

        has_ability? or has_conflicting_ability?

      %{"Passive" => ability} ->
        has_ability? = ability_id == ability.id

        has_conflicting_ability? =
          case ability.traits["RemoveSpells"] do
            nil ->
              false

            list ->
              ability_id in list
          end

        has_ability? or has_conflicting_ability?
    end)
  end

  def process_duration_traits(effects, target, caster) do
    effects
    |> Enum.reduce(effects, fn effect, updated_effects ->
      process_duration_trait(effect, updated_effects, target, caster)
    end)
  end

  def process_duration_trait(
        {"Damage", damages},
        %{"DamageShield" => _} = effects,
        _target,
        _caster
      ) do
    effects
    |> Map.put("DamageShield", %{})
    |> put_in(["DamageShield", "Damage"], damages)
    |> put_in(["DamageShield", "UserMessage"], effects["DamageShieldUserMessage"])
    |> put_in(["DamageShield", "TargetMessage"], effects["DamageShieldTargetMessage"])
    |> put_in(["DamageShield", "SpectatorMessage"], effects["DamageShieldSpectatorMessage"])
    |> Map.delete("Damage")
    |> Map.delete("DamageShieldUserMessage")
    |> Map.delete("DamageShieldTargetMessage")
    |> Map.delete("DamageShieldSpectatorMessage")
  end

  def process_duration_trait({"Damage", damages}, effects, _target, _caster)
      when is_float(damages) do
    effects
  end

  def process_duration_trait({"Damage", damages}, effects, target, caster) do
    damage_percent =
      Enum.reduce(damages, 0, fn
        %{kind: "physical", min: min, max: max, damage_type: type}, damage_percent ->
          min = trunc(min)
          max = trunc(max)

          ability_damage = Enum.random(min..max)

          bonus_damage = Mobile.ability_value(caster, "ModifyDamage")

          resist = Mobile.physical_resistance_at_level(target, target.level)

          resist = resist - Mobile.physical_penetration_at_level(caster, caster.level)

          resist_percent = 1 - resist / (target.level * 50 + resist)

          damage = (ability_damage + bonus_damage) * resist_percent

          modifier = Mobile.ability_value(target, "Resist#{type}")

          damage = damage * (1 - modifier / 100)

          percent = damage / Mobile.max_hp_at_level(target, target.level)

          damage_percent + percent

        %{kind: "magical", min: min, max: max, damage_type: type}, damage_percent ->
          min = trunc(min)
          max = trunc(max)

          ability_damage = Enum.random(min..max)

          bonus_damage = Mobile.ability_value(caster, "ModifyDamage")

          resist = Mobile.magical_resistance_at_level(target, target.level)

          resist = resist - Mobile.magical_penetration_at_level(caster, caster.level)

          resist_percent = 1 - resist / (target.level * 50 + resist)

          damage = (ability_damage + bonus_damage) * resist_percent

          modifier = Mobile.ability_value(target, "Resist#{type}")

          damage = damage * (1 - modifier / 100)

          percent = damage / Mobile.max_hp_at_level(target, target.level)

          damage_percent + percent

        %{kind: "drain", min: _min, max: _max, damage_type: _type}, damage_percent ->
          damage_percent
      end)

    effects
    |> Map.put("Damage", damage_percent)
  end

  def process_duration_trait({"AC%", percent}, effects, target, _caster) do
    ac_from_percent = ac_for_mitigation_at_level(percent, target.level)

    effects
    |> Map.put("AC", ac_from_percent)
    |> Map.delete("AC%")
  end

  def process_duration_trait({"MR%", percent}, effects, target, _caster) do
    mr_from_percent = ac_for_mitigation_at_level(percent, target.level)

    effects
    |> Map.put("MR", mr_from_percent)
    |> Map.delete("MR%")
  end

  def process_duration_trait({"Heal", value}, effects, target, _caster) do
    healing = (value["min"] + value["max"]) / 2

    percentage_healed = healing / Mobile.max_hp_at_level(target, target.level)

    effects
    |> Map.put("Heal", percentage_healed)
  end

  def process_duration_trait({"HealMana", value}, effects, target, caster) do
    level = min(target.level, caster.level)
    healing = Mobile.magical_penetration_at_level(caster, level) * (value / 100)

    percentage_healed =
      calculate_healing(healing, value) / Mobile.max_mana_at_level(target, level)

    effects
    |> Map.put("HealMana", percentage_healed)
    |> Map.put("Interval", Mobile.round_length_in_ms(caster) / 4)
    |> Map.put(
      "NextEffectAt",
      System.monotonic_time(:millisecond) + Mobile.round_length_in_ms(caster) / 4
    )
  end

  def process_duration_trait({trait, value}, effects, _target, _caster) do
    if trait in Map.values(effects) do
      put_in(effects[trait], value)
    else
      effects
    end
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
    |> Map.keys()
    |> Enum.member?(ability_name)
  end

  def apply_cooldowns(caster, %Ability{} = ability) do
    caster
    |> apply_ability_cooldown(ability)
  end

  def apply_ability_cooldown(caster, %Ability{cooldown: nil}), do: caster

  def apply_ability_cooldown(caster, %Ability{cooldown: cooldown, name: name}) do
    Systems.Effect.add(
      caster,
      %{
        "cooldown" => name,
        "RemoveMessage" => "#{name} is ready for use again."
      },
      cooldown
    )
  end

  def caster_cast_message(
        %Ability{result: :dodged} = ability,
        %{} = _caster,
        %{} = target,
        _mobile
      ) do
    message =
      ability.traits["DodgeUserMessage"]
      |> Text.interpolate(%{"target" => target, "ability" => ability.name})
      |> Text.capitalize_first()

    "<p><span class='dark-cyan'>#{message}</span></p>"
  end

  def caster_cast_message(
        %Ability{result: :blocked} = _ability,
        %{} = _caster,
        %{} = target,
        _mobile
      ) do
    shield = Character.shield(target).name

    message =
      "{{target}} blocks your attack with {{target:his/her/their}} #{shield}!"
      |> Text.interpolate(%{"target" => target})
      |> Text.capitalize_first()

    "<p><span class='dark-cyan'>#{message}</span></p>"
  end

  def caster_cast_message(
        %Ability{result: :parried} = _ability,
        %{} = _caster,
        %{} = target,
        _mobile
      ) do
    weapon = Character.weapon(target).name

    message =
      "{{target}} parries your attack with {{target:his/her/their}} #{weapon}!"
      |> Text.interpolate(%{"target" => target})
      |> Text.capitalize_first()

    "<p><span class='dark-cyan'>#{message}</span></p>"
  end

  def caster_cast_message(
        %Ability{result: :resisted} = ability,
        %{} = _caster,
        %{} = target,
        _mobile
      ) do
    message =
      @resist_message.user
      |> Text.interpolate(%{"target" => target, "ability" => ability.name})
      |> Text.capitalize_first()

    "<p><span class='dark-cyan'>#{message}</span></p>"
  end

  def caster_cast_message(
        %Ability{result: :deflected} = _ability,
        %{} = _caster,
        %{} = target,
        _mobile
      ) do
    message =
      @deflect_message.user
      |> Text.interpolate(%{"target" => target})
      |> Text.capitalize_first()

    "<p><span class='dark-red'>#{message}</span></p>"
  end

  def caster_cast_message(%Ability{} = ability, %{} = _caster, %Item{} = target, _mobile) do
    message =
      ability.user_message
      |> Text.interpolate(%{"target" => target})
      |> Text.capitalize_first()

    "<p><span class='#{message_color(ability)}'>#{message}</span></p>"
  end

  def caster_cast_message(
        %Ability{} = ability,
        %{} = _caster,
        %{ability_shift: nil} = target,
        _mobile
      ) do
    message =
      ability.user_message
      |> Text.interpolate(%{"target" => target})
      |> Text.capitalize_first()

    "<p><span class='#{message_color(ability)}'>#{message}</span></p>"
  end

  def caster_cast_message(
        %Ability{} = ability,
        %{} = caster,
        %{ability_shift: shift} = target,
        mobile
      ) do
    amount = -trunc(shift * Mobile.max_hp_at_level(target, mobile.level))

    cond do
      amount < 1 and has_ability?(ability, "Damage") ->
        if List.first(ability.traits["Damage"]).kind == "magical" do
          Map.put(ability, :result, :resisted)
        else
          Map.put(ability, :result, :deflected)
        end
        |> caster_cast_message(caster, target, mobile)

      :else ->
        message =
          ability.user_message
          |> Text.interpolate(%{"target" => target, "amount" => abs(amount)})
          |> Text.capitalize_first()

        "<p><span class='#{message_color(ability)}'>#{message}</span></p>"
    end
  end

  def target_cast_message(
        %Ability{result: :dodged} = ability,
        %{} = caster,
        %{} = _target,
        _mobile
      ) do
    message =
      ability.traits["DodgeTargetMessage"]
      |> Text.interpolate(%{"user" => caster, "ability" => ability.name})
      |> Text.capitalize_first()

    "<p><span class='dark-cyan'>#{message}</span></p>"
  end

  def target_cast_message(
        %Ability{result: :blocked} = _ability,
        %{} = caster,
        %{} = target,
        _mobile
      ) do
    shield = Character.shield(target).name

    message =
      "You block {{user}}'s attack with your #{shield}!"
      |> Text.interpolate(%{"user" => caster})
      |> Text.capitalize_first()

    "<p><span class='dark-cyan'>#{message}</span></p>"
  end

  def target_cast_message(
        %Ability{result: :parried} = _ability,
        %{} = caster,
        %{} = target,
        _mobile
      ) do
    weapon = Character.weapon(target).name

    message =
      "You parry {{user}}'s attack with your #{weapon}!"
      |> Text.interpolate(%{"user" => caster})
      |> Text.capitalize_first()

    "<p><span class='dark-cyan'>#{message}</span></p>"
  end

  def target_cast_message(
        %Ability{result: :resisted} = ability,
        %{} = caster,
        %{} = _target,
        _mobile
      ) do
    message =
      @resist_message.target
      |> Text.interpolate(%{"user" => caster, "ability" => ability.name})
      |> Text.capitalize_first()

    "<p><span class='dark-cyan'>#{message}</span></p>"
  end

  def target_cast_message(
        %Ability{result: :deflected} = _ability,
        %{} = caster,
        %{} = _target,
        _mobile
      ) do
    message =
      @deflect_message.target
      |> Text.interpolate(%{"user" => caster})
      |> Text.capitalize_first()

    "<p><span class='dark-red'>#{message}</span></p>"
  end

  def target_cast_message(
        %Ability{} = ability,
        %{} = caster,
        %{ability_shift: nil} = _target,
        _mobile
      ) do
    message =
      ability.target_message
      |> Text.interpolate(%{"user" => caster})
      |> Text.capitalize_first()

    "<p><span class='#{message_color(ability)}'>#{message}</span></p>"
  end

  def target_cast_message(
        %Ability{} = ability,
        %{} = caster,
        %{ability_shift: _shift} = target,
        mobile
      ) do
    amount = -trunc(target.ability_shift * Mobile.max_hp_at_level(target, mobile.level))

    cond do
      amount < 1 and has_ability?(ability, "Damage") ->
        if List.first(ability.traits["Damage"]).kind == "magical" do
          Map.put(ability, :result, :resisted)
        else
          Map.put(ability, :result, :deflected)
        end
        |> target_cast_message(caster, target, mobile)

      :else ->
        message =
          ability.target_message
          |> Text.interpolate(%{"user" => caster, "amount" => abs(amount)})
          |> Text.capitalize_first()

        "<p><span class='#{message_color(ability)}'>#{message}</span></p>"
    end
  end

  def spectator_cast_message(
        %Ability{result: :dodged} = ability,
        %{} = caster,
        %{} = target,
        _mobile
      ) do
    message =
      ability.traits["DodgeSpectatorMessage"]
      |> Text.interpolate(%{"user" => caster, "target" => target, "ability" => ability.name})
      |> Text.capitalize_first()

    "<p><span class='dark-cyan'>#{message}</span></p>"
  end

  def spectator_cast_message(
        %Ability{result: :blocked} = _ability,
        %{} = caster,
        %{} = target,
        _mobile
      ) do
    shield = Character.shield(target).name

    message =
      "{{target}} blocks {{user}}'s attack with {{target:his/her/their}} #{shield}!"
      |> Text.interpolate(%{"user" => caster, "target" => target})
      |> Text.capitalize_first()

    "<p><span class='dark-cyan'>#{message}</span></p>"
  end

  def spectator_cast_message(
        %Ability{result: :parried} = _ability,
        %{} = caster,
        %{} = target,
        _mobile
      ) do
    weapon = Character.weapon(target).name

    message =
      "{{target}} parries {{user}}'s attack with {{target:his/her/their}} #{weapon}!"
      |> Text.interpolate(%{"user" => caster, "target" => target})
      |> Text.capitalize_first()

    "<p><span class='dark-cyan'>#{message}</span></p>"
  end

  def spectator_cast_message(
        %Ability{result: :resisted} = ability,
        %{} = caster,
        %{} = target,
        _mobile
      ) do
    message =
      @resist_message.spectator
      |> Text.interpolate(%{"user" => caster, "target" => target, "ability" => ability.name})
      |> Text.capitalize_first()

    "<p><span class='dark-cyan'>#{message}</span></p>"
  end

  def spectator_cast_message(
        %Ability{result: :deflected} = _ability,
        %{} = caster,
        %{} = target,
        _mobile
      ) do
    message =
      @deflect_message.spectator
      |> Text.interpolate(%{"user" => caster, "target" => target})
      |> Text.capitalize_first()

    "<p><span class='dark-red'>#{message}</span></p>"
  end

  def spectator_cast_message(%Ability{} = ability, %{} = caster, %Item{} = target, _mobile) do
    message =
      ability.spectator_message
      |> Text.interpolate(%{"user" => caster, "target" => target})
      |> Text.capitalize_first()

    "<p><span class='#{message_color(ability)}'>#{message}</span></p>"
  end

  def spectator_cast_message(
        %Ability{} = ability,
        %{} = caster,
        %{ability_shift: nil} = target,
        _mobile
      ) do
    message =
      ability.spectator_message
      |> Text.interpolate(%{"user" => caster, "target" => target})
      |> Text.capitalize_first()

    "<p><span class='#{message_color(ability)}'>#{message}</span></p>"
  end

  def spectator_cast_message(
        %Ability{} = ability,
        %{} = caster,
        %{ability_shift: _shift} = target,
        mobile
      ) do
    amount = -trunc(target.ability_shift * Mobile.max_hp_at_level(target, mobile.level))

    cond do
      amount < 1 and has_ability?(ability, "Damage") ->
        if List.first(ability.traits["Damage"]).kind == "magical" do
          Map.put(ability, :result, :resisted)
        else
          Map.put(ability, :result, :deflected)
        end
        |> spectator_cast_message(caster, target, mobile)

      :else ->
        message =
          ability.spectator_message
          |> Text.interpolate(%{"user" => caster, "target" => target, "amount" => abs(amount)})
          |> Text.capitalize_first()

        "<p><span class='#{message_color(ability)}'>#{message}</span></p>"
    end
  end

  def display_cast_message(%Room{} = room, %{} = caster, %Item{} = target, %Ability{} = ability) do
    room.mobiles
    |> Map.values()
    |> Enum.each(fn mobile ->
      cond do
        mobile.ref == caster.ref and not is_nil(ability.user_message) ->
          Mobile.send_scroll(mobile, caster_cast_message(ability, caster, target, mobile))

        mobile && not is_nil(ability.spectator_message) ->
          Mobile.send_scroll(mobile, spectator_cast_message(ability, caster, target, mobile))

        true ->
          :noop
      end
    end)
  end

  def display_cast_message(%Room{} = room, %{} = caster, %{} = target, %Ability{} = ability) do
    room.mobiles
    |> Map.values()
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

  def display_pre_cast_message(
        %Room{} = room,
        %{} = caster,
        [target_ref | _rest] = targets,
        %Ability{traits: %{"PreCastMessage" => message}} = ability
      ) do
    target = Room.get_mobile(room, target_ref)

    message =
      message
      |> Text.interpolate(%{"target" => target})
      |> Text.capitalize_first()

    Mobile.send_scroll(caster, "<p><span class='#{message_color(ability)}'>#{message}</span></p>")

    display_pre_cast_message(
      room,
      caster,
      targets,
      update_in(ability.traits, &Map.delete(&1, "PreCastMessage"))
    )
  end

  def display_pre_cast_message(
        %Room{} = room,
        %{} = caster,
        [target_ref | _rest],
        %Ability{traits: %{"PreCastSpectatorMessage" => message}} = ability
      ) do
    target = Room.get_mobile(room, target_ref)

    message =
      message
      |> Text.interpolate(%{"user" => caster, "target" => target})
      |> Text.capitalize_first()

    Room.send_scroll(room, "<p><span class='#{message_color(ability)}'>#{message}</span></p>", [
      caster
    ])
  end

  def display_pre_cast_message(_room, _caster, _targets, _ability), do: :noop

  def message_color(%Ability{kind: kind}) when kind in ["attack", "critical"], do: "red"
  def message_color(%Ability{}), do: "blue"

  def can_execute?(%Room{} = room, mobile, ability) do
    cond do
      cd = on_cooldown?(mobile, ability) ->
        Mobile.send_scroll(
          mobile,
          "<p>#{ability.name} is on cooldown: #{time_remaining(mobile, cd)} seconds remaining.</p>"
        )

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

  def on_cooldown?(%{} = _mobile, %Ability{cooldown: nil} = _ability), do: false

  def on_cooldown?(%{effects: effects} = _mobile, %Ability{name: name} = _ability) do
    effects
    |> Map.values()
    |> Enum.find(&(&1["cooldown"] == name))
  end

  def get_targets(%Room{} = room, caster_ref, %Ability{targets: "monster or single"}, query) do
    caster = room.mobiles[caster_ref]

    match =
      room.mobiles
      |> Map.values()
      |> Enum.reject(&(&1.ref in Party.refs(room, caster)))
      |> Enum.reject(
        &(&1.sneaking && !(&1.ref in caster.detected_characters) && !(&1.ref == caster_ref))
      )
      |> Match.one(:name_contains, query)

    List.wrap(match && match.ref)
  end

  def get_targets(%Room{}, _caster_ref, %Ability{targets: "self"}, _query) do
    []
  end

  def get_targets(%Room{} = room, _caster_ref, %Ability{targets: "monster"}, query) do
    match =
      room.mobiles
      |> Map.values()
      |> Enum.filter(&(&1.__struct__ == Monster))
      |> Match.one(:name_contains, query)

    List.wrap(match && match.ref)
  end

  def get_targets(%Room{} = room, caster_ref, %Ability{targets: "full party area"}, "") do
    room
    |> Room.get_mobile(caster_ref)
    |> Mobile.party_refs(room)
  end

  def get_targets(%Room{}, _caster_ref, %Ability{targets: "full party area"}, _query) do
    []
  end

  def get_targets(%Room{} = room, caster_ref, %Ability{targets: "full attack area"}, "") do
    party =
      room
      |> Room.get_mobile(caster_ref)
      |> Mobile.party_refs(room)

    room.mobiles
    |> Map.keys()
    |> Kernel.--(party)
  end

  def get_targets(%Room{}, _caster_ref, %Ability{targets: "full attack area"}, _query) do
    []
  end

  def get_targets(%Room{}, _caster_ref, %Ability{targets: "self or single"}, "") do
    []
  end

  def get_targets(%Room{} = room, caster_ref, %Ability{targets: "self or single"}, query) do
    caster = room.mobiles[caster_ref]

    match =
      room.mobiles
      |> Map.values()
      |> Enum.reject(&(&1.__struct__ == Monster))
      |> Enum.reject(
        &(&1.sneaking && !(&1.ref in caster.detected_characters) && !(&1.ref == caster_ref))
      )
      |> Match.one(:name_contains, query)

    List.wrap(match && match.ref)
  end

  def get_targets(%Room{} = room, caster_ref, %Ability{targets: "single"}, query) do
    caster = room.mobiles[caster_ref]

    match =
      room.mobiles
      |> Map.values()
      |> Enum.reject(&(&1.__struct__ == Monster || &1.ref == caster_ref))
      |> Enum.reject(&(&1.sneaking && !(&1.ref in caster.detected_characters)))
      |> Match.one(:name_contains, query)

    List.wrap(match && match.ref)
  end

  def get_targets(%Room{} = room, caster_ref, %Ability{targets: target}, query)
      when target in ["weapon", "armour"] do
    %Character{inventory: inventory, equipment: equipment} = character = room.mobiles[caster_ref]

    (inventory ++ equipment)
    |> Enum.filter(&(&1.type == String.capitalize(target)))
    |> Match.all(:keyword_starts_with, query)
    |> case do
      nil ->
        []

      %Item{} = item ->
        item

      matches ->
        Mobile.send_scroll(
          character,
          "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"
        )

        Enum.each(matches, fn match ->
          Mobile.send_scroll(
            character,
            "<p>-- #{Item.colored_name(match, character: character)}</p>"
          )
        end)

        :too_many_matches
    end
  end

  def not_enough_mana?(%{} = _mobile, %Ability{ignores_round_cooldown?: true}), do: false

  def not_enough_mana?(%{} = mobile, %Ability{} = ability) do
    if !Mobile.enough_mana_for_ability?(mobile, ability) do
      Mobile.send_scroll(
        mobile,
        "<p><span class='cyan'>You do not have enough mana to use that ability.</span></p>"
      )
    end
  end

  def quality_too_high?(%Ability{} = ability, %Item{} = item) do
    quality = Trait.merge_traits(ability.traits, item.traits)["Quality"]

    max_quality = Item.max_quality(item)

    quality > max_quality
  end

  def casting_failed?(%{} = _caster, %Ability{difficulty: nil}), do: false

  def casting_failed?(%{} = caster, %Ability{difficulty: difficulty} = ability) do
    spellcasting = Mobile.spellcasting_at_level(caster, caster.level, ability)
    :rand.uniform(100) > spellcasting + difficulty
  end

  def casting_failed(room, caster_ref, ability) do
    room =
      Room.update_mobile(room, caster_ref, fn caster ->
        Mobile.send_scroll(
          caster,
          "<p><span class='dark-cyan'>You attempt to cast #{ability.name}, but fail.</span></p>"
        )

        Room.send_scroll(
          room,
          "<p><span class='dark-cyan'>#{caster.name} attempts to cast #{ability.name}, but fails.</span></p>",
          [caster]
        )

        ability = Map.put(ability, :mana, div(ability.mana, 2))

        caster
        |> Mobile.subtract_mana(ability)
        |> Mobile.subtract_energy(ability)
      end)

    Room.update_energy_bar(room, caster_ref)
    Room.update_mana_bar(room, caster_ref)

    room
  end
end
