defmodule ApathyDrive.Ability do
  use ApathyDriveWeb, :model

  alias ApathyDrive.{
    Ability,
    AbilityDamageType,
    AbilityTrait,
    Character,
    CraftingRecipe,
    Enchantment,
    Item,
    ItemInstance,
    Match,
    Mobile,
    Monster,
    Party,
    Regeneration,
    Repo,
    Room,
    RoomMonster,
    Scripts,
    Stealth,
    Text,
    TimerManager
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
    field(:letter, :string)

    field(:caster, :any, virtual: true)
    field(:limbs, :any, virtual: true, default: [])
    field(:traits, :map, virtual: true, default: %{})
    field(:ignores_round_cooldown?, :boolean, virtual: true, default: false)
    field(:result, :any, virtual: true)
    field(:cast_complete, :boolean, virtual: true, default: false)
    field(:skills, :any, virtual: true, default: [])
    field(:target_list, :any, virtual: true)
    field(:attributes, :any, virtual: true)
    field(:max_stacks, :integer, virtual: true, default: 1)
    field(:chance, :integer, virtual: true)
    field(:on_hit?, :boolean, virtual: true, default: false)
    field(:can_crit, :boolean, virtual: true, default: false)
    field(:spell?, :boolean, virtual: true, default: true)
    field(:reaction_energy, :integer, virtual: true, default: 0)
    field(:auto, :boolean, virtual: true, default: true)
    field(:class_id, :integer, virtual: true)
    field(:skill_id, :integer, virtual: true)

    belongs_to(:crit_table, ApathyDrive.DamageType)

    has_many(:monsters_abilities, ApathyDrive.MonsterAbility)
    has_many(:monsters, through: [:monsters_abilities, :monster])

    has_many(:death_monsters, ApathyDrive.Monster, foreign_key: :death_ability_id)

    has_many(:abilities_traits, ApathyDrive.AbilityTrait)
    has_many(:trait_records, through: [:abilities_traits, :trait])

    has_many(:abilities_damage_types, ApathyDrive.AbilityDamageType)
    has_many(:damage_types, through: [:abilities_damage_types, :damage_types])

    timestamps()
  end

  @required_fields ~w(name targets kind command description )a
  @optional_fields ~w(user_message target_message spectator_message duration cooldown energy mana difficulty level letter)a

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

  @kinds ["heal", "attack", "auto attack", "curse", "utility", "blessing", "passive", "long-term"]

  @instant_traits [
    "CurePoison",
    "Damage",
    "DispelMagic",
    "Enslave",
    "Freedom",
    "Heal",
    "HealMana",
    "KillSpell",
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
    "Alignment",
    "Bubble",
    "Bubble%",
    "Charm",
    "Blind",
    "Charm",
    "Confusion",
    "ConfusionMessage",
    "ConfusionSpectatorMessage",
    "Crits",
    "Damage",
    "DamageOverTime",
    "DamageShield",
    "DamageShieldUserMessage",
    "DamageShieldTargetMessage",
    "DamageShieldSpectatorMessage",
    "DarkVision",
    "DeusExMachina",
    "Dodge",
    "Encumbrance",
    "EndCast",
    "EndCast%",
    "Fear",
    "Heal",
    "Health",
    "HolyMission",
    "HPRegen",
    "Intellect",
    "Light",
    "LightVision",
    "MaxBubble",
    "MaxBubble%",
    "MaxHP",
    "MaxHP%",
    "MR",
    "MR%",
    "ManaRegen",
    "MaxHP",
    "MaxMana",
    "ModifyDamage",
    "Perception",
    "Picklocks",
    "Poison",
    "PoisonImmunity",
    "RemoveMessage",
    "ResistAether",
    "ResistCold",
    "ResistCrushing",
    "ResistCutting",
    "ResistDisruption",
    "ResistElectricity",
    "ResistFire",
    "ResistHoly",
    "ResistImpact",
    "ResistImpaling",
    "ResistInfernal",
    "ResistPlasma",
    "ResistPoison",
    "ResistSonic",
    "ResistStress",
    "ResistStrike",
    "ResistUnholy",
    "ResistVacuum",
    "RestoreLimbs",
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
    "Unbalanced",
    "Willpower",
    "WeaponDamage"
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

  def unbalance(room, mobile_ref) do
    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      if Mobile.has_ability?(mobile, "Unbalanced") do
        if :rand.uniform(100) < 20 do
          case :rand.uniform(4) do
            1 ->
              if mobile.__struct__ == Character do
                if weapon = Character.weapon(mobile) do
                  Mobile.send_scroll(
                    mobile,
                    "<p><span class='dark-yellow'>You clumsily drop your weapon!</span></p>"
                  )

                  ApathyDrive.Commands.Drop.drop_item(room, mobile, weapon)
                else
                  mobile
                end
              else
                mobile
              end

            2 ->
              ability = %Ability{
                kind: "attack",
                energy: 0,
                mana: 0,
                spell?: false,
                user_message: "You trip and fall!",
                spectator_message: "{{user}} trips and falls!",
                ignores_round_cooldown?: true,
                traits: %{
                  "Damage" => 0.10
                }
              }

              Ability.execute(room, mobile_ref, ability, [mobile_ref])

            3 ->
              ability = %Ability{
                kind: "attack",
                energy: 0,
                mana: 0,
                spell?: false,
                user_message: "You stumble into a wall!",
                spectator_message: "{{user}} stumbles into a wall!",
                ignores_round_cooldown?: true,
                traits: %{
                  "Damage" => 0.10
                }
              }

              Ability.execute(room, mobile_ref, ability, [mobile_ref])

            4 ->
              ability = %Ability{
                kind: "attack",
                energy: 0,
                mana: 0,
                spell?: false,
                user_message: "You collapse to the ground!",
                spectator_message: "{{user}} collapses to the ground!",
                ignores_round_cooldown?: true,
                traits: %{
                  "Damage" => 0.10
                }
              }

              Ability.execute(room, mobile_ref, ability, [mobile_ref])
          end
        else
          mobile
        end
      else
        mobile
      end
    end)
  end

  def ac_for_mitigation_at_level(mitigation_percent) do
    level = 25
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

  def appropriate_alignment?(%Ability{traits: traits}, %Character{} = mobile) do
    alignment = Character.alignment(mobile)

    cond do
      Map.has_key?(traits, "Good") and alignment != "good" ->
        false

      Map.has_key?(traits, "NotGood") and alignment == "good" ->
        false

      Map.has_key?(traits, "Neutral") and alignment != "neutral" ->
        false

      Map.has_key?(traits, "NotNeutral") and alignment == "neutral" ->
        false

      Map.has_key?(traits, "Evil") and alignment != "evil" ->
        false

      Map.has_key?(traits, "NotEvil") and alignment == "evil" ->
        false

      :else ->
        true
    end
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

  def set_name_changeset(model, name) do
    model
    |> cast(%{name: name}, [:name])
    |> validate_required(:name)
    |> validate_length(:name, min: 1, max: 25)
  end

  def set_duration_changeset(model, duration) do
    model
    |> cast(%{duration: duration}, [:duration])
    |> validate_required(:duration)
    |> validate_number(:duration, greater_than: 0)
  end

  def set_energy_changeset(model, energy) do
    model
    |> cast(%{energy: energy}, [:energy])
    |> validate_required(:energy)
    |> validate_number(:energy, greater_than_or_equal_to: 0)
  end

  def set_difficulty_changeset(model, difficulty) do
    model
    |> cast(%{difficulty: difficulty}, [:difficulty])
    |> validate_required(:difficulty)
    |> validate_number(:difficulty, [])
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

  def find(nil), do: nil

  def find(id) when is_integer(id) do
    ability = ApathyDrive.Repo.get(__MODULE__, id)

    if ability do
      load(ability)
    end
  end

  def find(id) do
    raise "error finding ability: #{inspect(id)}"
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
    if all do
      learnable = learnable_abilities()

      exact =
        Ability
        |> where(name: ^name)
        |> Repo.all()

      (learnable ++ exact)
      |> Enum.uniq_by(& &1.name)
      |> Match.all(:keyword_starts_with, name)
    else
      __MODULE__
      |> where([ability], not is_nil(ability.name) and ability.name != "")
      |> distinct(true)
      |> ApathyDrive.Repo.all()
      |> Match.all(:keyword_starts_with, name)
    end
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
    |> Enum.reject(&(is_nil(&1.name) or &1.name == "" or &1.kind == "base-class"))
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
    |> Enum.sort_by(&(-&1.level))
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
    Room.update_mobile(room, caster_ref, fn room, caster ->
      room =
        Room.update_mobile(room, caster.ref, fn _room, caster ->
          caster =
            caster
            |> Stealth.reveal()

          Mobile.update_prompt(caster, room)

          caster =
            if lt = Enum.find(TimerManager.timers(caster), &match?({:longterm, _}, &1)) do
              Character.send_chat(
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
              enchantment = Repo.preload(enchantment, :skill)
              enchantment = Repo.preload(enchantment, :items_instances)
              time = Enchantment.next_tick_time(caster, enchantment)

              Mobile.send_scroll(
                caster,
                "<p><span class='cyan'>You continue your work.</span></p>"
              )

              Mobile.send_scroll(
                caster,
                "<p><span class='dark-green'>Time Left:</span> <span class='dark-cyan'>#{
                  Enchantment.time_left(caster, enchantment) |> Enchantment.formatted_time_left()
                }</span></p>"
              )

              caster
              |> TimerManager.send_after(
                {{:longterm, item.instance_id}, :timer.seconds(time),
                 {:lt_tick, time, caster_ref, enchantment}}
              )
              |> Map.put(:enchantment, enchantment)

            [] ->
              recipe = CraftingRecipe.for_item(item)

              enchantment =
                %Enchantment{
                  items_instances_id: item.instance_id,
                  ability_id: nil,
                  skill_id: recipe.skill_id
                }
                |> Repo.insert!()
                |> Repo.preload(:items_instances)
                |> Repo.preload(:skill)

              time = Enchantment.next_tick_time(caster, enchantment)
              Mobile.send_scroll(caster, "<p><span class='cyan'>You begin work.</span></p>")

              Mobile.send_scroll(
                caster,
                "<p><span class='dark-green'>Time Left:</span> <span class='dark-cyan'>#{
                  Enchantment.time_left(caster, enchantment) |> Enchantment.formatted_time_left()
                }</span></p>"
              )

              caster
              |> TimerManager.send_after(
                {{:longterm, item.instance_id}, :timer.seconds(time),
                 {:lt_tick, time, caster_ref, enchantment}}
              )
              |> Map.put(:enchantment, enchantment)
          end
        end)

      Room.update_moblist(room)

      room
    end)
  end

  def execute(%Room{} = room, caster_ref, %Ability{kind: "long-term"} = ability, %Item{} = item) do
    Room.update_mobile(room, caster_ref, fn room, caster ->
      if Enchantment.max_stacks?(item, ability) do
        Mobile.send_scroll(
          caster,
          "<p><span class='dark-cyan'>This #{item.name} can receive no more #{ability.name} enchantments.</span></p>"
        )

        room
      else
        traits =
          ability.traits
          |> Map.update("RequireItems", [item.instance_id], &[item.instance_id | &1])
          |> Map.put(
            "TickMessage",
            "<p><span class='dark-cyan'>You continue enchanting the #{item.name}.</span></p>"
          )

        ability = Map.put(ability, :traits, traits)

        cond do
          mobile = not_enough_energy(caster, Map.put(ability, :target_list, item)) ->
            mobile

          casting_failed?(caster, ability) ->
            casting_failed(room, caster_ref, ability)

          can_execute?(room, caster, ability) ->
            display_pre_cast_message(room, caster, item, ability)

            room =
              Room.update_mobile(room, caster.ref, fn _room, caster ->
                caster =
                  caster
                  |> apply_cooldowns(ability)
                  |> Mobile.subtract_mana(ability)
                  |> Mobile.subtract_energy(ability)
                  |> Stealth.reveal()

                Mobile.update_prompt(caster, room)

                caster =
                  if lt = Enum.find(TimerManager.timers(caster), &match?({:longterm, _}, &1)) do
                    Character.send_chat(
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
                    time = Enchantment.next_tick_time(caster, enchantment)

                    Mobile.send_scroll(
                      caster,
                      "<p><span class='cyan'>You continue your work.</span></p>"
                    )

                    Mobile.send_scroll(
                      caster,
                      "<p><span class='dark-green'>Time Left:</span> <span class='dark-cyan'>#{
                        Enchantment.time_left(caster, enchantment)
                        |> Enchantment.formatted_time_left()
                      }</span></p>"
                    )

                    TimerManager.send_after(
                      caster,
                      {{:longterm, item.instance_id}, :timer.seconds(time),
                       {:lt_tick, time, caster_ref, enchantment}}
                    )
                    |> Map.put(:enchantment, enchantment)

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
            Room.update_mobile(room, caster_ref, fn _room, caster ->
              Mobile.subtract_energy(caster, ability)
            end)
        end
      end
    end)
  end

  def execute(%Room{} = room, caster_ref, %Ability{} = ability, %Item{} = item) do
    Room.update_mobile(room, caster_ref, fn room, caster ->
      cond do
        mobile = not_enough_energy(caster, Map.put(ability, :target_list, item)) ->
          mobile

        casting_failed?(caster, ability) ->
          casting_failed(room, caster_ref, ability)

        can_execute?(room, caster, ability) ->
          display_pre_cast_message(room, caster, item, ability)

          display_cast_message(room, caster, item, ability)

          caster =
            caster
            |> apply_cooldowns(ability)
            |> Mobile.subtract_mana(ability)
            |> Mobile.subtract_energy(ability)

          effects =
            ability.traits
            |> Map.take(@duration_traits)
            |> Map.put("stack_key", ability.traits["StackKey"] || ability.id)
            |> Map.put("stack_count", ability.traits["StackCount"] || 1)
            |> Map.put("effect_ref", make_ref())

          effects =
            if damage = effects["Damage"] do
              effects =
                if ability.traits["Elemental"] do
                  elemental_damage = Enum.find(damage, &(&1.damage_type == "Unaspected"))
                  damage = List.delete(damage, elemental_damage)

                  if lore = caster.lore do
                    elemental_damage =
                      Enum.map(lore.damage_types, fn damage ->
                        damage
                        |> Map.put(:min, elemental_damage.min)
                        |> Map.put(:max, elemental_damage.min)
                      end)

                    Map.put(effects, "Damage", elemental_damage ++ damage)
                  else
                    elemental_damage = Map.put(elemental_damage, :damage_type, "Elemental")
                    Map.put(effects, "Damage", [elemental_damage | damage])
                  end
                else
                  Map.put(effects, "Damage", damage)
                end

              effects
              |> Map.put("WeaponDamage", effects["Damage"])
              |> Map.delete("Damage")
            else
              effects
            end

          effects =
            if message = effects["RemoveMessage"] do
              message =
                message
                |> Text.interpolate(%{
                  "item" => item.name,
                  "lore" => caster.lore && caster.lore.name
                })

              Map.put(effects, "RemoveMessage", message)
            else
              effects
            end

          if "lock enchantment" in item.enchantments and !ability.traits["Script"] do
            lock_enchantment_id = Repo.get_by(Ability, name: "lock enchantment").id

            enchantment =
              Enchantment
              |> Repo.get_by(
                ability_id: lock_enchantment_id,
                items_instances_id: item.instance_id
              )

            value =
              if ability.traits["Elemental"] do
                caster.lore.name
              end

            enchantment
            |> Ecto.Changeset.change(%{
              ability_id: ability.id,
              value: value
            })
            |> Repo.update()

            Room.send_scroll(
              room,
              "<p><span class='blue'>An enchantment on the weapon flares, making the spell permanent.</span></p>"
            )

            Character.load_items(caster)
          else
            if script = ability.traits["Script"] do
              room
              |> Room.update_mobile(caster_ref, fn room, caster ->
                Module.safe_concat([Scripts, Macro.camelize(script)]).execute(
                  room,
                  caster.ref,
                  item
                )
              end)
            else
              item = Systems.Effect.add(item, effects, :timer.seconds(ability.duration))

              caster =
                if item.equipped do
                  location =
                    Enum.find_index(
                      caster.equipment,
                      &(&1.instance_id == item.instance_id)
                    )

                  update_in(caster.equipment, &List.replace_at(&1, location, item))
                else
                  location =
                    Enum.find_index(
                      caster.inventory,
                      &(&1.instance_id == item.instance_id)
                    )

                  update_in(caster.inventory, &List.replace_at(&1, location, item))
                end

              Mobile.update_prompt(caster, room)
              room = put_in(room.mobiles[caster_ref], caster)

              Room.update_energy_bar(room, caster_ref)
              Room.update_hp_bar(room, caster_ref)
              Room.update_mana_bar(room, caster_ref)

              Room.update_moblist(room)

              caster
            end
          end

        :else ->
          Room.update_mobile(room, caster_ref, fn _room, caster ->
            Mobile.subtract_energy(caster, ability)
          end)
      end
    end)
  end

  def execute(%Room{} = room, caster_ref, %Ability{} = ability, targets) when is_list(targets) do
    Room.update_mobile(room, caster_ref, fn room, caster ->
      cond do
        mobile = not_enough_energy(caster, Map.put(ability, :target_list, targets)) ->
          mobile

        casting_failed?(caster, ability) ->
          casting_failed(room, caster_ref, ability)

        can_execute?(room, caster, ability) ->
          display_pre_cast_message(room, caster, targets, ability)

          {caster, ability} = crit(caster, ability)

          room = put_in(room.mobiles[caster_ref], caster)

          room =
            Enum.reduce(targets, room, fn target_ref, updated_room ->
              Room.update_mobile(updated_room, target_ref, fn _room, target ->
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

                    put_in(updated_room.mobiles[target.ref], target)
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
            Room.update_mobile(room, caster.ref, fn _room, caster ->
              caster =
                caster
                |> apply_cooldowns(ability)
                |> Mobile.subtract_mana(ability)
                |> Mobile.subtract_energy(ability)

              Mobile.update_prompt(caster, room)

              if ability.kind in ["attack", "curse"] and !(caster.ref in targets) do
                if targets == [] do
                  caster
                else
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
                end
              else
                caster
              end
            end)

          Room.update_energy_bar(room, caster.ref)
          Room.update_hp_bar(room, caster.ref)
          Room.update_mana_bar(room, caster.ref)

          room =
            Room.update_mobile(room, caster_ref, fn _room, caster -> Stealth.reveal(caster) end)

          Room.update_moblist(room)

          room =
            if instance_id = ability.traits["DestroyItem"] do
              Room.update_mobile(room, caster_ref, fn _room, caster ->
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
          Room.update_mobile(room, caster_ref, fn _room, caster ->
            Mobile.subtract_energy(caster, ability)
          end)
      end
    end)
  end

  def start_enchantment(caster, item, ability) do
    enchantment =
      %Enchantment{items_instances_id: item.instance_id, ability_id: ability.id}
      |> Repo.insert!()
      |> Map.put(:ability, ability)

    time = Enchantment.next_tick_time(caster, enchantment)
    Mobile.send_scroll(caster, "<p><span class='cyan'>You begin work.</span></p>")

    Mobile.send_scroll(
      caster,
      "<p><span class='dark-green'>Time Left:</span> <span class='dark-cyan'>#{
        Enchantment.time_left(caster, enchantment) |> Enchantment.formatted_time_left()
      }</span></p>"
    )

    TimerManager.send_after(
      caster,
      {{:longterm, item.instance_id}, :timer.seconds(time),
       {:lt_tick, time, caster.ref, enchantment}}
    )
    |> Map.put(:enchantment, enchantment)
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

  def dodged?(%{} = caster, %{} = target, ability, room) do
    accuracy = Mobile.accuracy_at_level(caster, caster.level, room)

    accuracy_modifier =
      ability.limbs
      |> Enum.reduce(1.0, fn limb, modifier ->
        if caster.limbs do
          max(0, caster.limbs[limb].health) * modifier
        else
          modifier
        end
      end)

    accuracy = accuracy * accuracy_modifier

    dodge = Mobile.dodge_at_level(target, target.level, room)

    dodge_modifier =
      if Map.has_key?(target, :limbs) do
        target.limbs
        |> Enum.reduce(1.0, fn {_name, limb}, modifier ->
          if limb.type in ["leg", "foot"] do
            max(0, limb.health) * modifier
          else
            modifier
          end
        end)
      else
        1.0
      end

    modifier = Mobile.ability_value(target, "Dodge")

    dodge = (dodge + modifier) * dodge_modifier

    difference = dodge - accuracy

    chance =
      if difference > 0 do
        30 + difference * 0.3
      else
        30 + difference * 0.7
      end

    :rand.uniform(100) < chance
  end

  def blocked?(%{} = caster, %Character{} = target, ability, room) do
    if shield = Character.shield(target) do
      accuracy = Mobile.accuracy_at_level(caster, caster.level, room)

      limb_modifier =
        ability.limbs
        |> Enum.reduce(1.0, fn limb, modifier ->
          if caster.limbs do
            max(0, caster.limbs[limb].health) * modifier
          else
            modifier
          end
        end)

      accuracy = accuracy * limb_modifier

      block = Mobile.block_at_level(target, target.level)

      block_modifier =
        if Map.has_key?(target, :limbs) do
          max(0, target.limbs[shield.limb].health)
        else
          1.0
        end

      modifier = Mobile.ability_value(target, "Block")

      block = (block + modifier) * block_modifier

      difference = block - accuracy

      chance =
        if difference > 0 do
          30 + difference * 0.3
        else
          30 + difference * 0.7
        end

      :rand.uniform(100) < chance
    else
      false
    end
  end

  def blocked?(%{} = _caster, %{} = target, _ability, _room) do
    :rand.uniform(100) < Mobile.ability_value(target, "Block")
  end

  def parried?(%{} = caster, %Character{} = target, ability, room) do
    if weapon = Character.weapon(target) do
      accuracy = Mobile.accuracy_at_level(caster, caster.level, room)

      limb_modifier =
        ability.limbs
        |> Enum.reduce(1.0, fn limb, modifier ->
          if caster.limbs do
            max(0, caster.limbs[limb].health) * modifier
          else
            modifier
          end
        end)

      accuracy = accuracy * limb_modifier

      parry = Mobile.parry_at_level(target, target.level)

      limbs =
        if weapon.worn_on == "Two Handed" do
          ["left hand", "right hand"]
        else
          [weapon.limb]
        end

      parry_modifier =
        limbs
        |> Enum.reduce(1.0, fn limb, modifier ->
          max(0, target.limbs[limb].health) * modifier
        end)

      modifier = Mobile.ability_value(target, "Block")

      parry = (parry + modifier) * parry_modifier

      difference = parry - accuracy

      chance =
        if difference > 0 do
          30 + difference * 0.3
        else
          30 + difference * 0.7
        end

      :rand.uniform(100) < chance
    else
      false
    end
  end

  def parried?(%{} = _caster, %{} = target, _ability, _room) do
    :rand.uniform(100) < Mobile.ability_value(target, "Parry")
  end

  def apply_ability(
        %Room{} = room,
        %{} = caster,
        %{} = target,
        %Ability{traits: %{"Dodgeable" => true}} = ability
      ) do
    cond do
      dodged?(caster, target, ability, room) ->
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
          |> Map.put(:energy, min(target.max_energy, target.energy + 200))

        room = put_in(room.mobiles[target.ref], target)

        Room.update_energy_bar(room, target.ref)

        room
        |> apply_criticals(caster.ref, target.ref, ability)

      blocked?(caster, target, ability, room) ->
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

        room = put_in(room.mobiles[target.ref], target)

        caster = Map.put(caster, :energy, caster.energy - ability.energy)

        put_in(room.mobiles[caster.ref], caster)
        |> apply_criticals(caster.ref, target.ref, ability)

      parried?(caster, target, ability, room) ->
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

        room = put_in(room.mobiles[target.ref], target)

        riposte = Mobile.attack_ability(target, true)

        room
        |> apply_criticals(caster.ref, target.ref, ability)
        |> Ability.execute(target.ref, riposte, [caster.ref])

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

    # if companion = Character.companion(caster, room) do
    #   companion
    #   |> Companion.dismiss(room)
    #   |> Companion.convert_for_character(target, caster)
    # else
    #   Companion.convert_for_character(room, target, caster)
    # end
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

    if ability.kind == "curse" and ability.duration < 1 do
      Process.put(:ability_result, :resisted)
      display_cast_message(room, caster, target, Map.put(ability, :result, :resisted))

      target =
        target
        |> Map.put(:ability_shift, nil)
        |> Map.put(:ability_special, nil)
        |> Mobile.update_prompt(room)

      room
      |> put_in([:mobiles, target.ref], target)
    else
      display_cast_message(room, caster, target, ability)

      room
      |> trigger_damage_shields(caster.ref, target.ref, ability)
      |> apply_criticals(caster.ref, target.ref, ability)
      |> finish_ability(caster.ref, target.ref, ability, target.ability_shift)
    end
  end

  def apply_ability(%Room{} = room, _caster, _target, _ability), do: room

  def apply_criticals(%Room{} = room, caster_ref, target_ref, %Ability{kind: kind} = ability)
      when kind == "attack" do
    caster = room.mobiles[caster_ref]
    target = room.mobiles[target_ref]

    if caster && target do
      if crit = crit_for_damage(target.ability_shift, ability) do
        crit = put_in(crit.traits["StackCount"], 10)
        crit = Map.put(crit, :caster, ability.caster)

        target =
          if ability.traits["ConfusionMessage"] == "You are stunned and cannot move!" do
            target.effects
            |> Enum.filter(fn {_key, effect} ->
              Map.has_key?(effect, "Confusion")
            end)
            |> Enum.map(fn {key, _effect} -> key end)
            |> Enum.reduce(
              target,
              &Systems.Effect.remove(&2, &1,
                fire_after_cast: true,
                show_expiration_message: false
              )
            )
          else
            target
          end

        room = put_in(room.mobiles[target_ref], target)

        apply_ability(room, caster, target, crit)
      else
        room
      end
    else
      room
    end
  end

  def apply_criticals(%Room{} = room, _caster_ref, _target_ref, _ability), do: room

  def crit_for_damage(ability_shift, ability)
      when is_number(ability_shift) and ability_shift < 0 do
    crit_tables = Enum.map(ability.traits["Damage"], & &1.damage_type_id)

    percent = trunc(abs(ability_shift) * 100)

    crit_tables
    |> Enum.shuffle()
    |> Enum.find_value(fn table ->
      letter = roll_for_letter(percent)

      if letter do
        critical_ability(table, letter)
      end
    end)
  end

  def crit_for_damage(_ability_shift, _ability), do: nil

  def critical_ability(table, letter) do
    count =
      __MODULE__
      |> where(crit_table_id: ^table, letter: ^letter)
      |> select([crit], count(crit.id))
      |> Repo.one()

    __MODULE__
    |> where(crit_table_id: ^table, letter: ^letter)
    |> offset(fragment("floor(random()*?) LIMIT 1", ^count))
    |> select([crit], crit.id)
    |> Repo.one()
    |> find()
  end

  def roll_for_letter(crit_chance) do
    case :rand.uniform(1_000_000) do
      roll when roll > crit_chance * 10_000 ->
        nil

      roll when roll > crit_chance * 2500 ->
        "A"

      roll when roll > crit_chance * 625 ->
        "B"

      roll when roll > crit_chance * 150 ->
        "C"

      roll when roll > crit_chance * 50 ->
        "D"

      _ ->
        "E"
    end
  end

  def add_evil_points(room, %Ability{kind: kind} = ability, %Character{} = caster, target)
      when kind in ["attack", "auto attack", "curse"] do
    evil_points = Mobile.evil_points(target, caster)

    if evil_points > 0 do
      caster = Character.alter_evil_points(caster, evil_points)

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

  def limb(room, target_ref) do
    target = room.mobiles[target_ref]

    cond do
      is_nil(target) ->
        nil

      Map.has_key?(target, :limbs) ->
        target.limbs
        |> Map.keys()
        |> Enum.filter(&(target.limbs[&1].health > 0))
        |> case do
          [] ->
            nil

          limbs ->
            Enum.random(limbs)
        end

      :else ->
        nil
    end
  end

  def damage_limb(room, target_ref, limb_name, percentage, bleeding \\ false)

  def damage_limb(%Room{} = room, target_ref, limb_name, percentage, bleeding)
      when percentage < 0 do
    Room.update_mobile(room, target_ref, fn room, target ->
      if map_size(target.limbs) == 1 or Mobile.ability_value(target, "Bubble") > 0 do
        target
      else
        initial_limb_health = target.limbs[limb_name].health

        updated_health = initial_limb_health + percentage

        updated_health =
          if updated_health <= 0 do
            min(-0.25, updated_health)
          else
            updated_health
          end

        target = put_in(target.limbs[limb_name].health, updated_health)

        limb = target.limbs[limb_name]

        cond do
          updated_health <= 0 ->
            target =
              if !is_nil(limb[:parent]) do
                unless bleeding and limb[:fatal] do
                  Mobile.send_scroll(target, "<p>Your #{limb_name} is severed!</p>")

                  Room.send_scroll(
                    room,
                    "<p>#{Mobile.colored_name(target)}'s #{limb_name} is severed!</p>",
                    [target]
                  )
                end

                effect = %{
                  "StatusMessage" => "Your #{limb_name} is severed!",
                  "stack_key" => {:severed, limb_name}
                }

                target =
                  target
                  |> Systems.Effect.remove_oldest_stack({:crippled, limb_name})
                  |> Systems.Effect.add(effect)

                if Map.has_key?(target, :equipment) do
                  target.equipment
                  |> Enum.reduce(target, fn item_to_remove, target ->
                    if item_to_remove.limb == limb_name do
                      %ItemInstance{id: item_to_remove.instance_id}
                      |> Ecto.Changeset.change(%{
                        equipped: false,
                        class_id: nil
                      })
                      |> Repo.update!()

                      target = Character.load_items(target)

                      Mobile.send_scroll(
                        target,
                        "<p>You unequip your #{
                          Item.colored_name(item_to_remove, character: target)
                        }.</p>"
                      )

                      Room.send_scroll(
                        room,
                        "<p>#{target.name} unequipped their #{
                          Item.colored_name(item_to_remove, character: target)
                        }.</p>",
                        [target]
                      )

                      target
                    else
                      target
                    end
                  end)
                else
                  target
                end
              else
                target
              end

            if limb.fatal do
              if is_nil(limb[:parent]) and !bleeding do
                Mobile.send_scroll(
                  target,
                  "<p>You are dealt a mortal blow to the #{limb_name}!</p>"
                )

                Room.send_scroll(
                  room,
                  "<p>#{Mobile.colored_name(target)} is dealt a mortal blow to the #{limb_name}!</p>",
                  [target]
                )
              end

              put_in(room.mobiles[target.ref], target)
            else
              target =
                target
                |> Ecto.Changeset.change(%{
                  missing_limbs: [limb_name | target.missing_limbs]
                })
                |> Repo.update!()

              room = put_in(room.mobiles[target.ref], target)

              %ItemInstance{
                item_id: 1,
                room_id: room.id,
                equipped: false,
                hidden: false,
                name: limb_name <> " of " <> target.name,
                description: "This is the " <> limb_name <> " of " <> target.name <> ".",
                delete_at: Timex.shift(DateTime.utc_now(), minutes: 1)
              }
              |> Repo.insert!()

              room = Room.load_items(room)

              Enum.reduce(target.limbs, room, fn {other_limb_name, other_limb}, room ->
                if other_limb[:parent] == limb_name and other_limb.health > 0 do
                  damage_limb(room, target_ref, other_limb_name, -other_limb.health)
                else
                  room
                end
              end)
            end

          initial_limb_health >= 0.5 and limb.health < 0.5 and !limb.fatal ->
            Mobile.send_scroll(target, "<p>Your #{limb_name} is crippled!</p>")

            Room.send_scroll(
              room,
              "<p>#{Mobile.colored_name(target)}'s #{limb_name} is crippled!</p>",
              [target]
            )

            effect = %{
              "StatusMessage" => "Your #{limb_name} is crippled!",
              "stack_key" => {:crippled, limb_name}
            }

            target = Systems.Effect.add(target, effect)

            room = put_in(room.mobiles[target.ref], target)

            Enum.reduce(target.limbs, room, fn {other_limb_name, other_limb}, room ->
              if other_limb[:parent] == limb_name and other_limb.health > limb.health do
                amount = other_limb.health - limb.health

                damage_limb(room, target_ref, other_limb_name, -amount)
              else
                room
              end
            end)

          :else ->
            target
        end
      end
    end)
  end

  def damage_limb(%Room{} = room, _target_ref, _limb_name, _percentage, _bleeding), do: room

  def finish_ability(room, caster_ref, target_ref, ability, ability_shift) do
    room =
      Room.update_mobile(room, target_ref, fn room, target ->
        caster = room.mobiles[caster_ref]

        target =
          if ability_shift do
            {target, ability_shift} =
              if ability_shift < 0 do
                target.effects
                |> Enum.reduce({target, ability_shift}, fn
                  {id, %{"Bubble" => bubble} = effect}, {target, ability_shift} ->
                    cond do
                      bubble > abs(ability_shift) ->
                        target = update_in(target.effects[id]["Bubble"], &(&1 + ability_shift))
                        {target, 0}

                      bubble <= abs(ability_shift) ->
                        if effect["MaxBubble"] do
                          target = put_in(target.effects[id]["Bubble"], 0)
                          # target = Map.put(target, :effects, effects)
                          {target, ability_shift + bubble}
                        else
                          target =
                            Systems.Effect.remove(target, id, show_expiration_message: true)

                          {target, ability_shift + bubble}
                        end
                    end

                  {_id, _effect}, {target, ability_shift} ->
                    {target, ability_shift}
                end)
              else
                {target, ability_shift}
              end

            if ability_shift < 0 and Mobile.has_ability?(target, "HolyMission") do
              duration = :timer.seconds(30)
              rounds = duration / 5000

              Systems.Effect.add(
                target,
                %{
                  "Damage" => -ability_shift / rounds,
                  "StatusMessage" => "You are taking damage!",
                  "stack_key" => :holy_mission_damage,
                  "stack_count" => :infinity
                },
                duration
              )
            else
              initial_hp = target.hp

              target = Mobile.shift_hp(target, ability_shift)

              cond do
                initial_hp > 0 and target.hp < 0 and target.__struct__ == Character ->
                  Mobile.send_scroll(target, "<p>You lose conciousness!</p>")

                  Room.send_scroll(
                    room,
                    "<p>#{Mobile.colored_name(target)} loses conciousness!</p>",
                    [
                      target
                    ]
                  )

                  target

                initial_hp < 0 and target.hp > 0 ->
                  Mobile.send_scroll(target, "<p>You regain conciousness.</p>")

                  Room.send_scroll(
                    room,
                    "<p>#{Mobile.colored_name(target)} regains conciousness.</p>",
                    [
                      target
                    ]
                  )

                  target

                :else ->
                  target
              end
            end
          else
            target
          end

        target
        |> Map.put(:ability_shift, nil)
        |> Map.put(:ability_special, nil)
        |> apply_duration_traits(ability, caster)
        |> Mobile.update_prompt(room)
      end)

    target = room.mobiles[target_ref]

    room =
      cond do
        is_nil(target) ->
          room

        is_nil(ability_shift) ->
          room

        ability_shift > 0 ->
          Regeneration.heal_limbs(room, target_ref, ability_shift)

        (limb = limb(room, target_ref)) && !Mobile.has_ability?(target, "HolyMission") ->
          damage_limb(room, target_ref, limb, ability_shift * 2)

        :else ->
          room
      end

    room =
      if monster_id = ability.traits["Summon"] do
        caster = room.mobiles[caster_ref]
        monster = Repo.get!(Monster, monster_id)

        monster =
          %RoomMonster{
            room_id: room.id,
            monster_id: monster.id,
            level: caster.level,
            spawned_at: nil,
            zone_spawned_at: nil,
            delete_at: Map.get(caster, :delete_at),
            owner_id: Map.get(caster, :owner_id)
          }
          |> Monster.from_room_monster()

        Room.mobile_entered(room, monster, "")
      else
        room
      end

    room =
      if script = ability.traits["Script"] do
        Room.update_mobile(room, caster_ref, fn room, caster ->
          Module.safe_concat([Scripts, Macro.camelize(script)]).execute(
            room,
            caster.ref,
            target_ref
          )
        end)
      else
        room
      end

    room =
      if room.mobiles[target_ref] && ability.traits["CrippleLimb"] do
        ability.traits["CrippleLimb"]
        |> Enum.reduce(room, fn limb_type, room ->
          target = room.mobiles[target_ref]

          target.limbs
          |> Enum.filter(fn {_limb_name, limb} ->
            correct_limb_type =
              if limb_type == "non_fatal" do
                !limb.fatal
              else
                limb.type == limb_type
              end

            correct_limb_type and limb.health >= 0.5
          end)
          |> case do
            [] ->
              room

            limbs ->
              {limb_name, limb} = Enum.random(limbs)

              percentage = 0.25 - limb.health

              damage_limb(room, target_ref, limb_name, percentage)
          end
        end)
      else
        room
      end

    room =
      if room.mobiles[target_ref] && ability.traits["SeverLimb"] do
        ability.traits["SeverLimb"]
        |> Enum.reduce(room, fn limb_type, room ->
          target = room.mobiles[target_ref]

          target.limbs
          |> Enum.filter(fn {_limb_name, limb} ->
            correct_limb_type =
              if limb_type == "non_fatal" do
                !limb.fatal
              else
                limb.type == limb_type
              end

            correct_limb_type and limb.health > 0
          end)
          |> case do
            [] ->
              room

            limbs ->
              {limb_name, limb} = Enum.random(limbs)

              percentage = 0.0 - limb.health

              damage_limb(room, target_ref, limb_name, percentage)
          end
        end)
      else
        room
      end

    Room.update_hp_bar(room, target_ref)
    Room.update_hp_bar(room, caster_ref)
    Room.update_mana_bar(room, caster_ref)
    Room.update_mana_bar(room, target_ref)

    if (target = room.mobiles[target_ref]) && ability.traits["Kill"] do
      Mobile.die(target, room)
    else
      room
    end
  end

  def trigger_damage_shields(%Room{} = room, caster_ref, target_ref, _ability)
      when target_ref == caster_ref,
      do: room

  def trigger_damage_shields(%Room{} = room, _caster_ref, _target_ref, %Ability{kind: "critical"}),
    do: room

  def trigger_damage_shields(%Room{} = room, caster_ref, target_ref, ability) do
    if (target = room.mobiles[target_ref]) && ability.kind != "blessing" &&
         "Damage" in Map.keys(ability.traits) do
      target
      |> Map.get(:effects)
      |> Map.values()
      |> Enum.filter(&Map.has_key?(&1, "DamageShield"))
      |> Enum.reduce(room, fn
        %{"DamageShield" => percentage}, updated_room when is_integer(percentage) ->
          if is_number(target.ability_shift) and target.ability_shift < 0 do
            amount =
              -trunc(
                target.ability_shift *
                  Mobile.max_hp_at_level(target, room.mobiles[caster_ref].level)
              )

            reaction = %Ability{
              kind: "critical",
              mana: 0,
              energy: 0,
              user_message: ability.traits["DamageShieldUserMessage"],
              target_message: ability.traits["DamageShieldTargetMessage"],
              spectator_message: ability.traits["DamageShieldSpectatorMessage"],
              traits: %{
                "Damage" => [%{kind: "raw", min: amount, max: amount, damage_type: "Unaspected"}]
              }
            }

            apply_ability(
              updated_room,
              updated_room.mobiles[target_ref],
              updated_room.mobiles[caster_ref],
              reaction
            )
          else
            updated_room
          end

        %{"DamageShield" => shield}, updated_room ->
          case shield["Damage"] do
            [%{max: nil, min: nil} | _] = damages ->
              # roll = :rand.uniform(100)

              # chance = Mobile.crits_at_level(target, target.level)

              # if roll <= chance do
              caster = room.mobiles[caster_ref]

              display_cast_message(
                room,
                target,
                caster,
                %Ability{
                  user_message: shield["UserMessage"],
                  target_message: shield["TargetMessage"],
                  spectator_message: shield["SpectatorMessage"]
                }
              )

              damage = Enum.random(damages)
              letter = Enum.random(["A", "B", "C"])
              reaction = critical_ability(damage.damage_type_id, letter)

              apply_ability(
                updated_room,
                updated_room.mobiles[target_ref],
                updated_room.mobiles[caster_ref],
                reaction
              )

            # else
            #   updated_room
            # end

            damage ->
              reaction = %Ability{
                kind: "critical",
                mana: 0,
                energy: 0,
                reaction_energy: Enum.random(100..300),
                user_message: shield["UserMessage"],
                target_message: shield["TargetMessage"],
                spectator_message: shield["SpectatorMessage"],
                traits: %{
                  "Damage" => damage
                }
              }

              apply_ability(
                updated_room,
                updated_room.mobiles[target_ref],
                updated_room.mobiles[caster_ref],
                reaction
              )
          end
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
        Systems.Effect.remove_all_stacks(updated_target, ability_id)
      end)

    {caster, target}
  end

  def apply_instant_trait({"CurePoison", _args}, %{} = target, _ability, caster, _room) do
    target =
      target.effects
      |> Enum.filter(fn {_key, effect} ->
        Map.has_key?(effect, "Poison")
      end)
      |> Enum.map(fn {key, _effect} -> key end)
      |> Enum.reduce(
        target,
        &Systems.Effect.remove(&2, &1, fire_after_cast: true, show_expiration_message: true)
      )

    {caster, target}
  end

  def apply_instant_trait({"Freedom", _args}, %{} = target, _ability, caster, _room) do
    target =
      target.effects
      |> Enum.filter(fn {_key, effect} ->
        Map.has_key?(effect, "Root")
      end)
      |> Enum.map(fn {key, _effect} -> key end)
      |> Enum.reduce(
        target,
        &Systems.Effect.remove(&2, &1, fire_after_cast: true, show_expiration_message: true)
      )

    {caster, target}
  end

  def apply_instant_trait({"DispelMagic", trait_id}, %{} = target, _ability, caster, _room) do
    if trait_id == 0 do
      target =
        target.effects
        |> Enum.reject(fn {_key, effect} ->
          String.match?(to_string(effect["stack_key"]), ~r/character|item|class|race/)
        end)
        |> Enum.map(fn {key, _effect} -> key end)
        |> Enum.reduce(
          target,
          &Systems.Effect.remove(&2, &1, fire_after_cast: true, show_expiration_message: true)
        )

      {caster, target}
    else
      trait = Repo.get!(ApathyDrive.Trait, trait_id).name

      target =
        target.effects
        |> Enum.filter(fn {_key, effect} ->
          Map.has_key?(effect, trait)
        end)
        |> Enum.map(fn {key, _effect} -> key end)
        |> Enum.reduce(
          target,
          &Systems.Effect.remove(&2, &1, fire_after_cast: true, show_expiration_message: true)
        )

      {caster, target}
    end
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

  def apply_instant_trait({"Damage", damages}, %{} = target, ability, caster, room) do
    lore = Map.get(caster, :lore)

    damages =
      if ability.traits["Elemental"] && lore do
        min =
          damages
          |> Enum.map(& &1.min)
          |> Enum.sum()

        max =
          damages
          |> Enum.map(& &1.max)
          |> Enum.sum()

        damages =
          Enum.reduce(damages, [], fn type, damages ->
            type =
              type
              |> Map.put(:max, max(1, div(max, 2)))
              |> Map.put(:min, max(1, div(min, 2)))

            [type | damages]
          end)

        lore_min =
          min
          |> div(2)
          |> div(length(lore.damage_types))
          |> max(1)

        lore_max =
          max
          |> div(2)
          |> div(length(lore.damage_types))
          |> max(1)

        Enum.reduce(lore.damage_types, damages, fn type, damages ->
          type =
            type
            |> Map.put(:min, lore_min)
            |> Map.put(:max, lore_max)

          [type | damages]
        end)
      else
        damages
      end

    round_percent = (ability.reaction_energy || ability.energy) / caster.max_energy

    target =
      target
      |> Map.put(:ability_shift, 0)

    {caster, damage_percent, target} =
      Enum.reduce(damages, {caster, 0, target}, fn
        %{kind: "raw", min: min, max: max, damage_type: type}, {caster, damage_percent, target} ->
          damage = Enum.random(min..max)

          modifier = Mobile.ability_value(target, "Resist#{type}")

          damage = damage * (1 - modifier / 100)

          percent = damage / Mobile.max_hp_at_level(target, target.level)

          {caster, damage_percent + percent, target}

        %{kind: "physical", min: min, max: max, damage_type: type},
        {caster, damage_percent, target} ->
          min = trunc(min)
          max = trunc(max)

          ability_damage = Enum.random(min..max)

          bonus_damage = Mobile.ability_value(caster, "ModifyDamage") * round_percent

          resist = Mobile.physical_resistance_at_level(target, target.level)

          resist = resist - Mobile.physical_penetration_at_level(caster, caster.level)

          resist_percent = 1 - resist / (25 * 50 + resist)

          damage = (ability_damage + bonus_damage) * resist_percent

          modifier = Mobile.ability_value(target, "Resist#{type}")

          damage = damage * (1 - modifier / 100)

          percent = damage / Mobile.max_hp_at_level(target, target.level)

          {caster, damage_percent + percent, target}

        %{kind: "physical", damage: dmg, damage_type: type}, {caster, damage_percent, target} ->
          resist = Mobile.physical_resistance_at_level(target, target.level)

          resist_percent = 1 - resist / (25 * 50 + resist)

          damage = dmg * resist_percent

          modifier = Mobile.ability_value(target, "Resist#{type}")

          damage = damage * (1 - modifier / 100)

          percent = damage / Mobile.max_hp_at_level(target, target.level)

          {caster, damage_percent + percent, target}

        %{kind: "magical", min: min, max: max, damage_type: type},
        {caster, damage_percent, target} ->
          min = trunc(min)
          max = trunc(max)

          ability_damage = Enum.random(min..max)

          bonus_damage = Mobile.ability_value(caster, "ModifyDamage") * round_percent

          resist = Mobile.magical_resistance_at_level(target, target.level)

          resist = resist - Mobile.magical_penetration_at_level(caster, caster.level)

          resist_percent = 1 - resist / (25 * 50 + resist)

          damage = (ability_damage + bonus_damage) * resist_percent

          modifier = Mobile.ability_value(target, "Resist#{type}")

          damage = damage * (1 - modifier / 100)

          max_hp = Mobile.max_hp_at_level(target, target.level)

          percent = damage / max_hp

          {caster, damage_percent + percent, target}

        %{kind: "magical", damage: damage, damage_type: type}, {caster, damage_percent, target} ->
          resist = Mobile.magical_resistance_at_level(target, target.level)

          resist_percent = 1 - resist / (25 * 50 + resist)

          damage = damage * resist_percent

          modifier = Mobile.ability_value(target, "Resist#{type}")

          damage = damage * (1 - modifier / 100)

          percent = damage / Mobile.max_hp_at_level(target, target.level)

          {caster, damage_percent + percent, target}

        %{kind: "drain", min: min, max: max, damage_type: type},
        {caster, damage_percent, target} ->
          damage = Enum.random(min..max)

          modifier = Mobile.ability_value(target, "Resist#{type}")

          damage = damage * (1 - modifier / 100)

          percent = damage / Mobile.max_hp_at_level(target, target.level)

          heal_percent = damage / Mobile.max_hp_at_level(caster, caster.level)

          caster = Mobile.shift_hp(caster, heal_percent)

          Mobile.update_prompt(caster, room)

          {caster, damage_percent + percent, target}

        %{kind: "mana-drain", min: min, max: max}, {caster, damage_percent, target} ->
          damage = Enum.random(min..max)

          percent = damage / Mobile.max_mana_at_level(target, target.level)

          heal_percent = damage / Mobile.max_mana_at_level(caster, caster.level)

          caster =
            caster
            |> update_in([Access.key!(:mana)], &min(1.0, &1 + heal_percent))

          target =
            target
            |> update_in([Access.key!(:mana)], &max(0, &1 - percent))

          Mobile.update_prompt(caster, room)

          {caster, damage_percent + percent, target}
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
      caster =
        Character.add_attribute_experience(caster, %{
          intellect: 0.5,
          charm: 0.5
        })

      ability =
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

      {caster, ability}
    else
      {caster, ability}
    end
  end

  def crit(caster, ability), do: {caster, ability}

  def calculate_healing(damage, modifier) do
    damage * (modifier / 100) * (Enum.random(95..105) / 100)
  end

  def duration_traits(%Ability{kind: "critical"} = ability) do
    ability.traits
    |> Map.take(@duration_traits)
    |> Map.delete("Damage")
  end

  def duration_traits(%Ability{} = ability) do
    ability.traits
    |> Map.take(@duration_traits)
  end

  def duration_traits(%{} = traits) do
    traits
    |> Map.take(@duration_traits)
  end

  def apply_duration_traits(%{} = target, %Ability{} = _ability, nil), do: target

  def apply_duration_traits(%{} = target, %Ability{duration: duration} = ability, %{} = caster)
      when is_integer(duration) and duration > 0 do
    effects =
      ability
      |> duration_traits()
      |> Map.put("stack_key", ability.traits["StackKey"] || ability.id)
      |> Map.put("stack_count", ability.traits["StackCount"] || 1)
      |> process_duration_traits(target, caster, ability.duration)
      |> Map.put("effect_ref", make_ref())

    if message = effects["StatusMessage"] do
      Mobile.send_scroll(
        target,
        "<p><span style='color: #{message_color(ability, target, :target)};'>#{message}</span></p>"
      )
    end

    if has_passive_ability?(target, ability.id) do
      target
    else
      target
      |> Systems.Effect.add(effects, :timer.seconds(ability.duration))
    end
  end

  def apply_duration_traits(%{} = target, _ability, _caster), do: target

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

  def process_duration_traits(effects, target, caster, duration) do
    effects
    |> Enum.reduce(effects, fn effect, updated_effects ->
      process_duration_trait(effect, updated_effects, target, caster, duration)
    end)
  end

  def process_duration_trait(
        {"Damage", damages},
        %{"DamageShield" => true} = effects,
        _target,
        _caster,
        _duration
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

  def process_duration_trait({"DamageOverTime", damage}, effects, target, _caster, duration) do
    rounds = :timer.seconds(duration) / 5000

    percent = damage / Mobile.max_hp_at_level(target, target.level)

    percent = percent / rounds

    effects
    |> Map.delete("DamageOverTime")
    |> Map.put("Damage", percent)
    |> Map.put("StatusMessage", "You are taking damage!")
  end

  def process_duration_trait({"Poison", damage}, effects, target, _caster, duration) do
    if Mobile.has_ability?(target, "PoisonImmunity") do
      Map.put(effects, "Damage", 0)
    else
      modifier = Mobile.ability_value(target, "ResistPoison")

      damage = damage * (1 - modifier / 100)

      rounds = :timer.seconds(duration) / 5000

      percent = damage / Mobile.max_hp_at_level(target, target.level)

      percent = percent / rounds

      Map.put(effects, "Damage", percent)
    end
  end

  def process_duration_trait({"Damage", damages}, effects, _target, _caster, _duration)
      when is_float(damages) do
    effects
  end

  def process_duration_trait({"Damage", damages}, effects, target, caster, _duration) do
    damage_percent =
      Enum.reduce(damages, 0, fn
        %{kind: "raw", min: min, max: max, damage_type: type}, damage_percent ->
          damage = Enum.random(min..max)

          modifier = Mobile.ability_value(target, "Resist#{type}")

          damage = damage * (1 - modifier / 100)

          percent = damage / Mobile.max_hp_at_level(target, target.level)

          damage_percent + percent

        %{kind: "physical", min: min, max: max, damage_type: type}, damage_percent ->
          min = trunc(min)
          max = trunc(max)

          ability_damage = Enum.random(min..max)

          bonus_damage = Mobile.ability_value(caster, "ModifyDamage")

          resist = Mobile.physical_resistance_at_level(target, target.level)

          resist = resist - Mobile.physical_penetration_at_level(caster, caster.level)

          resist_percent = 1 - resist / (25 * 50 + resist)

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

          resist_percent = 1 - resist / (25 * 50 + resist)

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

  def process_duration_trait({"AC%", percent}, effects, _target, _caster, _duration) do
    ac_from_percent = ac_for_mitigation_at_level(percent)

    effects
    |> Map.put("AC", ac_from_percent)
    |> Map.delete("AC%")
  end

  def process_duration_trait({"MR%", percent}, effects, _target, _caster, _duration) do
    mr_from_percent = ac_for_mitigation_at_level(percent)

    effects
    |> Map.put("MR", mr_from_percent)
    |> Map.delete("MR%")
  end

  def process_duration_trait({"Heal", value}, effects, target, _caster, _duration) do
    healing = (value["min"] + value["max"]) / 2

    percentage_healed = healing / Mobile.max_hp_at_level(target, target.level)

    effects
    |> Map.put("Heal", percentage_healed)
  end

  def process_duration_trait(
        {"Bubble", %{"min" => min, "max" => max}},
        effects,
        target,
        caster,
        duration
      ) do
    bubble = (min + max) / 2

    process_duration_trait({"Bubble", bubble}, effects, target, caster, duration)
  end

  def process_duration_trait({"Bubble", value}, effects, target, _caster, _duration) do
    percentage = value / Mobile.max_hp_at_level(target, target.level)

    effects
    |> Map.put("Bubble", percentage)
  end

  def process_duration_trait(
        {"MaxBubble", %{"min" => min, "max" => max}},
        effects,
        target,
        caster,
        duration
      ) do
    bubble = (min + max) / 2

    process_duration_trait({"MaxBubble", bubble}, effects, target, caster, duration)
  end

  def process_duration_trait({"MaxBubble", value}, effects, target, _caster, _duration) do
    percentage = value / Mobile.max_hp_at_level(target, target.level)

    effects
    |> Map.put("MaxBubble", percentage)
  end

  def process_duration_trait(
        {"Bubble%", %{"min" => min, "max" => max}},
        effects,
        target,
        caster,
        duration
      ) do
    bubble = (min + max) / 2

    process_duration_trait({"Bubble%", bubble}, effects, target, caster, duration)
  end

  def process_duration_trait({"Bubble%", value}, effects, _target, _caster, _duration) do
    percentage = value / 100

    effects
    |> Map.delete("Bubble%")
    |> Map.put("Bubble", percentage)
  end

  def process_duration_trait(
        {"MaxBubble%", %{"min" => min, "max" => max}},
        effects,
        target,
        caster,
        duration
      ) do
    bubble = (min + max) / 2

    process_duration_trait({"MaxBubble%", bubble}, effects, target, caster, duration)
  end

  def process_duration_trait({"MaxBubble%", value}, effects, _target, _caster, _duration) do
    percentage = value / 100

    effects
    |> Map.delete("MaxBubble%")
    |> Map.put("MaxBubble", percentage)
  end

  def process_duration_trait(
        {"BubbleRegen%PerSecond", _value},
        effects,
        _target,
        _caster,
        _duration
      ) do
    effects
    |> Map.put("MaxBubble", Map.get(effects, "Bubble"))
  end

  def process_duration_trait({"HealMana", value}, effects, target, caster, _duration) do
    level = min(target.level, caster.level)
    healing = Mobile.magical_penetration_at_level(caster, level) * (value / 100)

    percentage_healed =
      calculate_healing(healing, value) / Mobile.max_mana_at_level(target, level)

    effects
    |> Map.put("HealMana", percentage_healed)
    |> Map.put("Interval", 1250)
    |> Map.put(
      "NextEffectAt",
      System.monotonic_time(:millisecond) + 1250
    )
  end

  def process_duration_trait({trait, value}, effects, _target, _caster, _duration) do
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
      :timer.seconds(cooldown)
    )
  end

  def caster_cast_message(
        %Ability{result: :dodged} = ability,
        %{} = caster,
        %{} = target,
        mobile
      ) do
    if target.ref == caster.ref && ability.caster do
      target_cast_message(ability, caster, target, mobile)
    else
      message =
        ability.traits["DodgeUserMessage"]
        |> Text.interpolate(%{"target" => target, "ability" => ability.name})
        |> Text.capitalize_first()

      "<p><span class='dark-cyan'>#{message}</span></p>"
    end
  end

  def caster_cast_message(
        %Ability{result: :blocked} = ability,
        %{} = caster,
        %{} = target,
        mobile
      ) do
    if target.ref == caster.ref && ability.caster do
      target_cast_message(ability, caster, target, mobile)
    else
      shield = Character.shield(target).name

      message =
        "{{target}} blocks your attack with {{target:his/her/their}} #{shield}!"
        |> Text.interpolate(%{"target" => target})
        |> Text.capitalize_first()

      "<p><span class='dark-cyan'>#{message}</span></p>"
    end
  end

  def caster_cast_message(
        %Ability{result: :parried} = ability,
        %{} = caster,
        %{} = target,
        mobile
      ) do
    if target.ref == caster.ref && ability.caster do
      target_cast_message(ability, caster, target, mobile)
    else
      weapon = Character.weapon(target).name

      message =
        "{{target}} parries your attack with {{target:his/her/their}} #{weapon}!"
        |> Text.interpolate(%{"target" => target})
        |> Text.capitalize_first()

      "<p><span class='dark-cyan'>#{message}</span></p>"
    end
  end

  def caster_cast_message(
        %Ability{result: :resisted} = ability,
        %{} = caster,
        %{} = target,
        mobile
      ) do
    if target.ref == caster.ref && ability.caster do
      target_cast_message(ability, caster, target, mobile)
    else
      message =
        @resist_message.user
        |> Text.interpolate(%{"target" => target, "ability" => ability.name})
        |> Text.capitalize_first()

      "<p><span class='dark-cyan'>#{message}</span></p>"
    end
  end

  def caster_cast_message(
        %Ability{result: :deflected} = ability,
        %{} = caster,
        %{} = target,
        mobile
      ) do
    if target.ref == caster.ref && ability.caster do
      target_cast_message(ability, caster, target, mobile)
    else
      message =
        @deflect_message.user
        |> Text.interpolate(%{"target" => target})
        |> Text.capitalize_first()

      "<p><span class='dark-red'>#{message}</span></p>"
    end
  end

  def caster_cast_message(%Ability{} = ability, %{} = caster, %Item{} = target, _mobile) do
    message =
      ability.user_message
      |> Text.interpolate(%{"target" => target, "lore" => caster.lore.name})
      |> Text.capitalize_first()

    "<p><span style='color: #{message_color(ability, caster, :caster)};'>#{message}</span></p>"
  end

  def caster_cast_message(
        %Ability{} = ability,
        %{} = caster,
        %{ability_shift: nil} = target,
        mobile
      ) do
    if target.ref == caster.ref && ability.caster do
      target_cast_message(ability, caster, target, mobile)
    else
      message =
        ability.user_message
        |> Text.interpolate(%{"target" => target})
        |> Text.capitalize_first()

      "<p><span style='color: #{message_color(ability, caster, :caster)};'>#{message}</span></p>"
    end
  end

  def caster_cast_message(
        %Ability{} = ability,
        %{} = caster,
        %{ability_shift: shift} = target,
        mobile
      ) do
    if target.ref == caster.ref && ability.caster do
      target_cast_message(ability, caster, target, mobile)
    else
      amount = -trunc(shift * Mobile.max_hp_at_level(target, mobile.level))

      cond do
        amount < 1 and has_ability?(ability, "Damage") and ability.kind != "critical" ->
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

          "<p><span style='color: #{message_color(ability, caster, :caster)};'>#{message}</span></p>"
      end
    end
  end

  def target_cast_message(
        %Ability{result: :dodged} = ability,
        %{} = caster,
        %{} = _target,
        _mobile
      ) do
    caster = ability.caster || caster

    message =
      ability.traits["DodgeTargetMessage"]
      |> Text.interpolate(%{"user" => caster, "ability" => ability.name})
      |> Text.capitalize_first()

    "<p><span class='dark-cyan'>#{message}</span></p>"
  end

  def target_cast_message(
        %Ability{result: :blocked} = ability,
        %{} = caster,
        %{} = target,
        _mobile
      ) do
    caster = ability.caster || caster
    shield = Character.shield(target).name

    message =
      "You block {{user}}'s attack with your #{shield}!"
      |> Text.interpolate(%{"user" => caster})
      |> Text.capitalize_first()

    "<p><span class='dark-cyan'>#{message}</span></p>"
  end

  def target_cast_message(
        %Ability{result: :parried} = ability,
        %{} = caster,
        %{} = target,
        _mobile
      ) do
    caster = ability.caster || caster
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
        %Ability{result: :deflected} = ability,
        %{} = caster,
        %{} = _target,
        _mobile
      ) do
    caster = ability.caster || caster

    message =
      @deflect_message.target
      |> Text.interpolate(%{"user" => caster})
      |> Text.capitalize_first()

    "<p><span class='dark-red'>#{message}</span></p>"
  end

  def target_cast_message(
        %Ability{} = ability,
        %{} = caster,
        %{ability_shift: nil} = target,
        _mobile
      ) do
    caster = ability.caster || caster

    message =
      ability.target_message
      |> Text.interpolate(%{"user" => caster})
      |> Text.capitalize_first()

    "<p><span style='color: #{message_color(ability, target, :target)};'>#{message}</span></p>"
  end

  def target_cast_message(
        %Ability{} = ability,
        %{} = caster,
        %{ability_shift: _shift} = target,
        mobile
      ) do
    caster = ability.caster || caster
    amount = -trunc(target.ability_shift * Mobile.max_hp_at_level(target, mobile.level))

    cond do
      amount < 1 and has_ability?(ability, "Damage") and ability.kind != "critical" ->
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

        "<p><span style='color: #{message_color(ability, target, :target)};'>#{message}</span></p>"
    end
  end

  def spectator_cast_message(
        %Ability{result: :dodged} = ability,
        %{} = caster,
        %{} = target,
        _mobile
      ) do
    caster = ability.caster || caster

    message =
      ability.traits["DodgeSpectatorMessage"]
      |> Text.interpolate(%{"user" => caster, "target" => target, "ability" => ability.name})
      |> Text.capitalize_first()

    "<p><span class='dark-cyan'>#{message}</span></p>"
  end

  def spectator_cast_message(
        %Ability{result: :blocked} = ability,
        %{} = caster,
        %{} = target,
        _mobile
      ) do
    caster = ability.caster || caster
    shield = Character.shield(target).name

    message =
      "{{target}} blocks {{user}}'s attack with {{target:his/her/their}} #{shield}!"
      |> Text.interpolate(%{"user" => caster, "target" => target})
      |> Text.capitalize_first()

    "<p><span class='dark-cyan'>#{message}</span></p>"
  end

  def spectator_cast_message(
        %Ability{result: :parried} = ability,
        %{} = caster,
        %{} = target,
        _mobile
      ) do
    caster = ability.caster || caster
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
    caster = ability.caster || caster

    message =
      @resist_message.spectator
      |> Text.interpolate(%{"user" => caster, "target" => target, "ability" => ability.name})
      |> Text.capitalize_first()

    "<p><span class='dark-cyan'>#{message}</span></p>"
  end

  def spectator_cast_message(
        %Ability{result: :deflected} = ability,
        %{} = caster,
        %{} = target,
        _mobile
      ) do
    caster = ability.caster || caster

    message =
      @deflect_message.spectator
      |> Text.interpolate(%{"user" => caster, "target" => target})
      |> Text.capitalize_first()

    "<p><span class='dark-red'>#{message}</span></p>"
  end

  def spectator_cast_message(%Ability{} = ability, %{} = caster, %Item{} = target, mobile) do
    caster = ability.caster || caster

    message =
      ability.spectator_message
      |> Text.interpolate(%{"user" => caster, "target" => target})
      |> Text.capitalize_first()

    "<p><span style='color: #{message_color(ability, mobile, :spectator)};'>#{message}</span></p>"
  end

  def spectator_cast_message(
        %Ability{} = ability,
        %{} = caster,
        %{ability_shift: nil} = target,
        mobile
      ) do
    caster = ability.caster || caster

    message =
      ability.spectator_message
      |> Text.interpolate(%{"user" => caster, "target" => target})
      |> Text.capitalize_first()

    "<p><span style='color: #{message_color(ability, mobile, :spectator)};'>#{message}</span></p>"
  end

  def spectator_cast_message(
        %Ability{} = ability,
        %{} = caster,
        %{ability_shift: _shift} = target,
        mobile
      ) do
    caster = ability.caster || caster

    amount = -trunc(target.ability_shift * Mobile.max_hp_at_level(target, mobile.level))

    cond do
      amount < 1 and has_ability?(ability, "Damage") and ability.kind != "critical" ->
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

        "<p><span style='color: #{message_color(ability, mobile, :spectator)};'>#{message}</span></p>"
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

    Mobile.send_scroll(
      caster,
      "<p><span style='color: #{message_color(ability, caster, :caster)};'>#{message}</span></p>"
    )

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

    Room.send_scroll(
      room,
      "<p><span style='color: #{message_color(ability, target, :spectator)};'>#{message}</span></p>",
      [
        caster
      ]
    )
  end

  def display_pre_cast_message(_room, _caster, _targets, _ability), do: :noop

  def message_color(%Ability{kind: kind}, %Character{} = character, :caster)
      when kind in ["attack", "critical"] do
    character.attack_color
  end

  def message_color(%Ability{kind: kind}, %Character{} = character, :spectator)
      when kind in ["attack", "critical"],
      do: character.spectator_color

  def message_color(%Ability{kind: kind}, %Character{} = character, :target)
      when kind in ["attack", "critical"],
      do: character.target_color

  def message_color(%Ability{kind: kind}, _mobile, _perspective)
      when kind in ["attack", "critical"],
      do: "red"

  def message_color(%Ability{kind: "curse"}, _, :target), do: "olive"

  def message_color(%Ability{}, _, _), do: "blue"

  def can_execute?(%Room{} = room, mobile, ability) do
    cond do
      Map.get(mobile, :death_ability_id, :none) == ability.id ->
        true

      lore_missing(mobile, ability) ->
        false

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

  def lore_missing(mobile, ability) do
    cond do
      !ability.traits["Elemental"] ->
        false

      Map.get(mobile, :lore) ->
        false

      :else ->
        Mobile.send_scroll(
          mobile,
          "<p><span class='red'>Elemental spells require an active lore! (see \"help lores\")</span></p>"
        )

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
    |> ApathyDrive.AI.pets_and_party(room.mobiles[caster_ref])
    |> Enum.map(& &1.ref)
  end

  def get_targets(%Room{}, _caster_ref, %Ability{targets: "full party area"}, _query) do
    []
  end

  def get_targets(%Room{} = room, caster_ref, %Ability{targets: "full attack area"}, "") do
    party =
      room
      |> ApathyDrive.AI.pets_and_party(room.mobiles[caster_ref])
      |> Enum.map(& &1.ref)

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

    target = String.capitalize(target)

    targets = if target == "Armour", do: ["Shield", "Armour"], else: [target]

    (inventory ++ equipment)
    |> Enum.filter(&(&1.type in targets))
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

  def casting_failed?(%{} = _caster, %Ability{difficulty: nil}), do: false

  def casting_failed?(%{} = caster, %Ability{difficulty: difficulty} = ability) do
    spellcasting = Mobile.spellcasting_at_level(caster, caster.level, ability)
    :rand.uniform(100) > spellcasting + difficulty
  end

  def casting_failed(room, caster_ref, ability) do
    room =
      Room.update_mobile(room, caster_ref, fn room, caster ->
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
