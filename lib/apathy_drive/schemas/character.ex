defmodule ApathyDrive.Character do
  use Ecto.Schema
  use ApathyDriveWeb, :model

  alias ApathyDrive.{
    Ability,
    AbilityDamageType,
    AbilityTrait,
    AI,
    ChannelHistory,
    Character,
    CharacterClass,
    CharacterMaterial,
    CharacterRace,
    CharacterSkill,
    CharacterTrait,
    Class,
    Companion,
    Currency,
    Directory,
    Regeneration,
    Item,
    Level,
    LimbSet,
    Monster,
    Mobile,
    Party,
    RaceTrait,
    Room,
    RoomServer,
    Skill,
    Statix,
    Text,
    TimerManager,
    Title,
    Trait
  }

  require Logger
  import Comeonin.Bcrypt

  @behaviour Access
  defdelegate get_and_update(container, key, fun), to: Map
  defdelegate fetch(container, key), to: Map
  defdelegate get(container, key, default), to: Map
  defdelegate pop(container, key), to: Map

  schema "characters" do
    field(:name, :string)
    field(:gender, :string)
    field(:email, :string)
    field(:password, :string)
    field(:exp_buffer, :integer, default: 0)
    field(:timers, :map, virtual: true, default: %{})
    field(:admin, :boolean)
    field(:copper, :integer, default: 0)
    field(:silver, :integer, default: 0)
    field(:gold, :integer, default: 0)
    field(:platinum, :integer, default: 0)
    field(:runic, :integer, default: 0)
    field(:experience, :integer, default: 0)
    field(:race_id, :integer)
    field(:pity_modifier, :integer, default: 0)
    field(:auto_heal, :boolean)
    field(:auto_bless, :boolean)
    field(:auto_curse, :boolean)
    field(:auto_nuke, :boolean)
    field(:auto_roam, :boolean)
    field(:auto_flee, :boolean)
    field(:evil_points, :float)
    field(:last_evil_action_at, :utc_datetime_usec)
    field(:missing_limbs, {:array, :string}, default: [])
    field(:attack_color, :string, default: "red")
    field(:target_color, :string, default: "red")
    field(:spectator_color, :string, default: "red")

    field(:next_drain_at, :integer, virtual: true)
    field(:lore, :any, virtual: true)
    field(:level, :integer, virtual: true)
    field(:race, :any, virtual: true)
    field(:classes, :any, virtual: true)
    field(:monitor_ref, :any, virtual: true)
    field(:ref, :any, virtual: true)
    field(:socket, :any, virtual: true)
    field(:effects, :map, virtual: true, default: %{})
    field(:last_effect_key, :integer, virtual: true, default: 0)
    field(:hp, :float, virtual: true, default: 1.0)
    field(:mana, :float, virtual: true, default: 1.0)
    field(:abilities, :map, virtual: true, default: %{})
    field(:inventory, :any, virtual: true, default: [])
    field(:equipment, :any, virtual: true, default: [])
    field(:ability_shift, :float, virtual: true)
    field(:ability_special, :float, virtual: true)
    field(:attack_target, :any, virtual: true)
    field(:strength, :integer, virtual: true)
    field(:agility, :integer, virtual: true)
    field(:intellect, :integer, virtual: true)
    field(:willpower, :integer, virtual: true)
    field(:health, :integer, virtual: true)
    field(:charm, :integer, virtual: true)
    field(:leader, :any, virtual: true)
    field(:invitees, :any, virtual: true, default: [])
    field(:skills, :map, virtual: true, default: %{})
    field(:editing, :any, virtual: true)
    field(:attribute_levels, :any, virtual: true, default: %{})
    field(:energy, :integer, virtual: true, default: 1000)
    field(:max_energy, :integer, virtual: true, default: 1000)
    field(:reply_to, :string, virtual: true)
    field(:casting, :any, virtual: true, default: nil)
    field(:last_tick_at, :any, virtual: true)
    field(:last_room_id, :integer, virtual: true)
    field(:delayed, :boolean, virtual: true, default: false)
    field(:sneaking, :boolean, virtual: true, default: false)
    field(:detected_characters, :any, virtual: true, default: MapSet.new())
    field(:kill_counts, :map, virtual: true, default: %{})
    field(:materials, :map, virtual: true, default: %{})
    field(:death_race_id, :integer, virtual: true)
    field(:limbs, :map, virtual: true, default: %{})

    belongs_to(:room, Room)

    has_many(:items_instances, ApathyDrive.ItemInstance)

    has_many(:characters_skills, ApathyDrive.CharacterSkill)
    has_many(:trained_skills, through: [:characters_skills, :skill])

    has_many(:characters_materials, CharacterMaterial)

    has_many(:character_classes, ApathyDrive.CharacterClass)

    timestamps()
  end

  def top_list(number \\ 10) do
    CharacterClass
    |> Ecto.Query.order_by(desc: :level)
    |> Ecto.Query.preload([:class, :character])
    |> ApathyDrive.Repo.all()
    |> Enum.map(
      &%{
        name: &1.character.name,
        class: &1.class.name,
        level: &1.level,
        exp: &1.character.experience
      }
    )
    |> Enum.group_by(& &1.name)
    |> Enum.map(fn {_name, list} ->
      Enum.max_by(list, & &1.level)
    end)
    |> Enum.sort_by(&{&1.exp}, &>=/2)
    |> Enum.take(number)
  end

  def load_traits(%Character{} = character) do
    effect =
      character.id
      |> CharacterTrait.load_traits()
      |> Ability.process_duration_traits(character, character, nil)
      |> Map.put("stack_key", "character")
      |> Map.put("stack_count", 1)

    character
    |> Systems.Effect.add(effect)
  end

  def legal_status(%Character{evil_points: points}) when points >= 210, do: "FIEND"
  def legal_status(%Character{evil_points: points}) when points >= 120, do: "Villain"
  def legal_status(%Character{evil_points: points}) when points >= 80, do: "Criminal"
  def legal_status(%Character{evil_points: points}) when points >= 40, do: "Outlaw"
  def legal_status(%Character{evil_points: points}) when points >= 30, do: "Seedy"
  def legal_status(%Character{evil_points: points}) when points > -50, do: "Neutral"
  def legal_status(%Character{evil_points: points}) when points > -200, do: "Good"
  def legal_status(%Character{}), do: "Saint"

  def alignment(character) do
    case legal_status(character) do
      status when status in ["Saint", "Good"] ->
        "good"

      "Neutral" ->
        "neutral"

      _ ->
        "evil"
    end
  end

  def reduce_evil_points?(%Character{last_evil_action_at: nil}), do: true

  def reduce_evil_points?(%Character{last_evil_action_at: time}) do
    one_day_ago = Timex.shift(DateTime.utc_now(), days: -1)

    :lt == DateTime.compare(time, one_day_ago)
  end

  def set_title(%Character{} = character) do
    Map.put(character, :title, Title.for_character(character))
  end

  def modified_experience(%Character{} = character, exp) do
    modifier = 1 / (character.race.race.exp_modifier / 100)
    trunc(exp * modifier)
  end

  def max_active_abilities(%Character{level: level}) do
    5 + div(level, 5)
  end

  def add_currency_from_monster(character, monster) do
    currency_value = Monster.loot_wealth_in_copper(monster)

    if currency_value > 0 do
      currency = Currency.set_value(currency_value)

      Mobile.send_scroll(character, "<p>You receive #{Currency.to_string(currency)}.</p>")

      character
      |> Ecto.Changeset.change(%{
        runic: character.runic + currency.runic,
        platinum: character.platinum + currency.platinum,
        gold: character.gold + currency.gold,
        silver: character.silver + currency.silver,
        copper: character.copper + currency.copper
      })
      |> Repo.update!()
      |> Character.add_attribute_experience(%{charm: 1})
    else
      character
    end
  end

  def encumbrance(%Character{} = character) do
    Enum.reduce(character.equipment ++ character.inventory, 0, &(&1.weight + &2))
  end

  def max_encumbrance(%Character{} = character) do
    trunc(
      Mobile.attribute_at_level(character, :strength, character.level) * 48 *
        (1 + Systems.Effect.effect_bonus(character, "Encumbrance") / 100)
    )
  end

  def companion(%Character{id: id}, %Room{} = room) do
    room.mobiles
    |> Map.values()
    |> Enum.find(&(Map.get(&1, :character_id) == id))
  end

  def changeset(character, params \\ %{}) do
    character
    |> cast(params, ~w(name race_id gender))
    |> validate_required(~w(name race_id gender)a)
    |> validate_inclusion(:race_id, ApathyDrive.Race.ids())
    |> validate_inclusion(:gender, ["male", "female"])
    |> validate_format(:name, ~r/^[a-zA-Z]+$/)
    |> unique_constraint(:name, name: :characters_lower_name_index, on: Repo)
    |> validate_length(:name, min: 1, max: 12)
  end

  def sign_up_changeset(character, params \\ %{}) do
    character
    |> cast(params, ~w(email password name race_id gender)a)
    |> validate_required(~w(email password name race_id gender)a)
    |> validate_format(:email, ~r/@/)
    |> validate_length(:email, min: 3, max: 100)
    |> validate_length(:password, min: 6)
    |> unique_constraint(:email, name: :characters_lower_email_index, on: Repo)
    |> validate_confirmation(:password)
    |> validate_inclusion(:race_id, ApathyDrive.Race.ids())
    |> validate_inclusion(:gender, ["male", "female"])
    |> validate_format(:name, ~r/^[a-zA-Z]+$/)
    |> unique_constraint(:name, name: :characters_lower_name_index, on: Repo)
    |> validate_length(:name, min: 1, max: 12)
  end

  def load_abilities(%Character{id: id} = character) do
    character =
      character
      |> Map.put(:abilities, %{})

    auto_abilities =
      ApathyDrive.CharacterAbility
      |> Ecto.Query.where([ca], ca.character_id == ^id)
      |> Repo.all()
      |> Enum.reduce(%{}, fn auto_ability, auto_abilities ->
        Map.put(auto_abilities, auto_ability.ability_id, auto_ability.auto)
      end)

    class_abilities =
      Enum.reduce(character.classes, [], fn %{class_id: class_id, level: level}, abilities ->
        Enum.uniq(ApathyDrive.ClassAbility.abilities_at_level(class_id, level) ++ abilities)
      end)

    skill_abilities =
      Enum.map(character.skills, fn {_name, %CharacterSkill{skill_id: id, level: level}} ->
        ApathyDrive.SkillAbility.abilities_at_level(id, level)
      end)
      |> List.flatten()

    granted_abilities =
      character
      |> Mobile.ability_value("Grant")
      |> Enum.uniq()
      |> Enum.map(&%{ability: Ability.find(&1)})

    character =
      (class_abilities ++ skill_abilities ++ granted_abilities)
      |> Enum.reduce(character, fn
        %{ability: %Ability{id: id, kind: "base-class", level: level}}, character ->
          effect =
            id
            |> AbilityTrait.load_traits()
            |> Enum.reduce(%{}, fn {key, val}, effect ->
              effect
              |> Map.put(key, val * level)
              |> Map.put("stack_key", id)
              |> Map.put("stack_count", 1)
              |> Map.put("ClassLevel", level)
            end)
            |> Ability.process_duration_traits(character, character, nil)

          Systems.Effect.add(character, effect)

        %{ability: %Ability{id: id, kind: "passive"}}, character ->
          effect = AbilityTrait.load_traits(id)
          Systems.Effect.add(character, effect)

        %{ability: %Ability{id: id} = ability}, character ->
          ability =
            ability
            |> put_in([Access.key!(:traits)], AbilityTrait.load_traits(id))
            |> put_in([Access.key!(:auto)], !!auto_abilities[id])

          ability =
            case AbilityDamageType.load_damage(id) do
              [] ->
                ability

              damage ->
                ability = update_in(ability.traits, &Map.put(&1, "Damage", damage))

                crit_types = Enum.map(ability.traits["Damage"], & &1.damage_type_id)

                Map.put(ability, :crit_tables, crit_types)
            end

          if Ability.appropriate_alignment?(ability, character) do
            update_in(character.abilities, fn abilities ->
              abilities
              |> Map.put(ability.command, ability)
            end)
          else
            character
          end
      end)

    Enum.reduce(character.equipment, character, fn item, character ->
      if ability = item.traits["Grant"] do
        update_in(character.abilities, fn abilities ->
          Map.put(abilities, ability.command, ability)
        end)
      else
        character
      end
    end)
  end

  def set_attribute_levels(%Character{} = character) do
    [:strength, :agility, :intellect, :willpower, :health, :charm]
    |> Enum.reduce(character, fn stat, character ->
      exp = get_in(character, [Access.key!(:race), Access.key!(:"#{stat}_experience")])

      modifier = (100 + character.race.race.exp_modifier) / 100

      level = Level.level_at_exp(exp, modifier)

      character =
        character
        |> put_in([Access.key!(:attribute_levels), stat], level)
        |> put_in(
          [Access.key!(stat)],
          get_in(character, [Access.key!(:race), Access.key!(:race), Access.key!(stat)]) + level
        )

      Character.update_attribute_bar(character, stat)
      character
    end)
  end

  def set_skill_levels(%Character{} = character) do
    Skill
    |> Repo.all()
    |> Enum.reduce(character, fn %Skill{id: id, name: name, exp_multiplier: multiplier},
                                 character ->
      case Repo.get_by(CharacterSkill, skill_id: id, character_id: character.id) do
        nil ->
          skill =
            Repo.insert!(%CharacterSkill{
              character_id: character.id,
              skill_id: id,
              experience: 0,
              exp_multiplier: multiplier
            })
            |> Skill.set_level()

          put_in(character.skills[name], skill)

        %CharacterSkill{} = skill ->
          skill =
            skill
            |> Map.put(:exp_multiplier, multiplier)
            |> Skill.set_level()

          put_in(character.skills[name], skill)
      end
    end)
  end

  def load_classes(%Character{} = character) do
    classes =
      character
      |> Ecto.assoc(:character_classes)
      |> Repo.all()

    character
    |> Map.put(:classes, classes)
    |> set_level()
  end

  def set_level(%Character{classes: []} = character) do
    Map.put(character, :level, 0)
  end

  def set_level(%Character{classes: classes} = character) do
    level = Enum.reduce(classes, 0, &(&1.level + &2))
    Map.put(character, :level, level)
  end

  def load_race(%Character{race_id: race_id} = character) do
    character_race =
      CharacterRace
      |> Repo.get_by(%{character_id: character.id, race_id: race_id})
      |> case do
        %CharacterRace{} = character_race ->
          character_race

        nil ->
          %CharacterRace{character_id: character.id, race_id: race_id}
          |> Repo.insert!()
      end
      |> Repo.preload(:race)

    effect =
      race_id
      |> RaceTrait.load_traits()
      |> Ability.process_duration_traits(character, character, nil)
      |> Map.put("stack_key", "race")
      |> Map.put("stack_count", 1)

    character
    |> Map.put(:race, character_race)
    |> Systems.Effect.add(effect)
  end

  def load_limbs(%Character{race: %{race: race}} = character) do
    limbs = LimbSet.load_limbs(character, race.limb_set_id)

    limbs
    |> Enum.reduce(character, fn {limb_name, limb}, character ->
      if limb.health == 0 do
        effect = %{
          "StatusMessage" => "Your #{limb_name} is severed!",
          "stack_key" => {:severed, limb_name}
        }

        Systems.Effect.add(character, effect)
      else
        character
      end
    end)
    |> Map.put(:limbs, limbs)
  end

  def load_materials(%Character{} = character) do
    CharacterMaterial.load_for_character(character)
  end

  def load_items(%Character{} = character) do
    items = ApathyDrive.ItemInstance.load_items(character)

    character
    |> Map.put(:inventory, Enum.reject(items, & &1.equipped))
    |> Map.put(:equipment, Enum.filter(items, & &1.equipped))
    |> assign_limbs_to_equipment()
    |> add_equipped_items_effects()
    |> load_abilities()
    |> TimerManager.send_after(
      {:use_light_source, :timer.seconds(30), {:use_light_source, character.ref}}
    )
  end

  def assign_limbs_to_equipment(%Character{} = character) do
    Enum.reduce(character.equipment, character, fn item, character ->
      if is_nil(item.limb) do
        limbs = ApathyDrive.Commands.Wear.limbs_for_slot(character, item.worn_on)

        limb =
          Enum.find(limbs, fn limb ->
            if character.limbs[limb].health <= 0 do
              false
            else
              items_for_slot =
                character.equipment
                |> Enum.filter(&(&1.worn_on == item.worn_on))
                |> Enum.reject(&(&1 == item))

              Enum.all?(items_for_slot, &(&1.limb != limb))
            end
          end)

        equipment = List.delete(character.equipment, item)

        item = Map.put(item, :limb, limb)

        equipment = [item | equipment]

        put_in(character.equipment, equipment)
      else
        character
      end
    end)
  end

  def add_equipped_items_effects(%Character{} = character) do
    character =
      Enum.reduce(character.effects, character, fn
        {_key, %{"stack_key" => key}}, character ->
          if is_binary(key) and String.starts_with?(key, "item") do
            Systems.Effect.remove_oldest_stack(character, key)
          else
            character
          end

        _, character ->
          character
      end)

    Enum.reduce(character.equipment, character, fn item, updated_character ->
      add_equipped_item_effects(updated_character, item)
    end)
  end

  def add_equipped_item_effects(%Character{} = character, item) do
    {abilities, traits} =
      case item.traits do
        %{"Passive" => abilities} when is_list(abilities) ->
          Enum.reduce(abilities, {[], item.traits}, fn ability, {abilities, traits} ->
            traits = Trait.merge_traits(traits, ability.traits)
            {[ability | abilities], traits}
          end)

        %{"Passive" => ability} ->
          traits = Trait.merge_traits(item.traits, ability.traits)

          {[ability], traits}

        traits ->
          {nil, Ability.process_duration_traits(traits, character, character, nil)}
      end

    effect = Map.put(traits, "stack_key", "item-#{item.instance_id}")

    effect =
      if "DamageShield" in Map.keys(effect) do
        Ability.process_duration_trait({"Damage", effect["Damage"]}, effect, nil, nil, nil)
      else
        effect
      end

    ac = Item.ac_for_character(character, item)
    mr = Item.mr_for_character(character, item)

    effect =
      effect
      |> Map.put_new("AC", 0)
      |> update_in(["AC"], &div(&1 + ac, 2))
      |> Map.put_new("MR", 0)
      |> update_in(["MR"], &div(&1 + mr, 2))

    effect =
      if "Heal" in Map.keys(traits) do
        Ability.process_duration_trait(
          {"Heal", traits["Heal"]},
          effect,
          character,
          character,
          nil
        )
      else
        effect
      end

    if abilities do
      Enum.reduce(abilities, character, fn ability, character ->
        character
        |> Systems.Effect.remove_oldest_stack(ability.id)
      end)
      |> Systems.Effect.add(effect)
    else
      character
      |> Systems.Effect.add(effect)
    end
  end

  def sanitize(message) do
    {:safe, message} = Phoenix.HTML.html_escape(message)

    to_string(message)
  end

  def weapon(%Character{} = character) do
    character.equipment
    |> Enum.filter(&(&1.type == "Weapon"))
    |> case do
      [] ->
        nil

      list ->
        Enum.random(list)
    end
  end

  def ability_for_weapon(character, weapon, riposte) do
    verbs = %{
      "beat" => "Crushing",
      "bludgeon" => "Crushing",
      "chop" => "Cutting",
      "claw" => "Cutting",
      "cleave" => "Cutting",
      "clobber" => "Crushing",
      "crush" => "Crushing",
      "cut" => "Cutting",
      "double-shoot two arrows at" => "Impaling",
      "hack" => "Cutting",
      "hurl your chakram and strike" => "Cutting",
      "hurl your nexus spear at" => "Impaling",
      "hurl your shuriken and strike" => "Cutting",
      "hurl your throwing hammer and strike" => "Impact",
      "hurl your throwing knife and strike" => "Impaling",
      "impale" => "Impaling",
      "impale your nexus spear into" => "Impaling",
      "jab" => "Impaling",
      "lash" => "Cutting",
      "pierce" => "Impaling",
      "pound" => "Crushing",
      "rip" => "Cutting",
      "shoot a bolt at" => "Impaling",
      "shoot an arrow and strike" => "Impaling",
      "skewer" => "Impaling",
      "slam" => "Crushing",
      "slash" => "Cutting",
      "slice" => "Cutting",
      "slice and dice" => "Cutting",
      "smack" => "Impact",
      "smash" => "Crushing",
      "stab" => "Impaling",
      "whap" => "Crushing",
      "whip" => "Cutting"
    }

    {min_damage, max_damage} =
      if weapon.weapon_type do
        skill_level = get_in(character.skills, [weapon.weapon_type, Access.key!(:level)]) || 0

        target_damage = Item.target_damage(weapon.weapon_type, skill_level)

        damage = weapon_damage(weapon.speed, target_damage, skill_level)

        min_damage = (weapon.min_damage + damage.min_damage) / 2
        max_damage = (weapon.max_damage + damage.max_damage) / 2

        {min_damage, max_damage}
      else
        {weapon.min_damage, weapon.max_damage}
      end

    %Item{
      type: "Weapon",
      name: name,
      hit_verbs: hit_verbs,
      miss_verbs: [singular_miss, plural_miss],
      limb: limb
    } = weapon

    limbs = if limb, do: [limb], else: ["left hand", "right hand"]

    bonus_damage = Systems.Effect.effect_bonus(weapon, "WeaponDamage")

    [singular_hit, plural_hit] = Enum.random(hit_verbs)

    table = verbs[singular_hit] || "Crushing"

    [singular_hit, plural_hit] =
      if riposte do
        ["riposte", "ripostes"]
      else
        [singular_hit, plural_hit]
      end

    energy = Character.energy_per_swing(character, weapon)

    ability = %Ability{
      kind: "attack",
      energy: if(riposte, do: 0, else: energy),
      name: weapon.name,
      attributes: ["agility", "strength"],
      mana: 0,
      spell?: false,
      user_message: "You #{singular_hit} {{target}} with your #{name} for {{amount}} damage!",
      target_message: "{{user}} #{plural_hit} you with their #{name} for {{amount}} damage!",
      spectator_message:
        "{{user}} #{plural_hit} {{target}} with their #{name} for {{amount}} damage!",
      ignores_round_cooldown?: true,
      can_crit: true,
      traits: %{
        "Damage" => [
          %{
            kind: "physical",
            damage_type: table,
            min: min_damage,
            max: max_damage,
            damage_type_id: Repo.get_by(ApathyDrive.DamageType, name: table).id
          }
          | bonus_damage
        ],
        "Dodgeable" => if(riposte, do: false, else: true),
        "DodgeUserMessage" =>
          "You #{singular_miss} {{target}} with your #{name}, but they dodge!",
        "DodgeTargetMessage" => "{{user}} #{plural_miss} you with their #{name}, but you dodge!",
        "DodgeSpectatorMessage" =>
          "{{user}} #{plural_miss} {{target}} with their #{name}, but they dodge!"
      },
      limbs: limbs,
      skills: [weapon.weapon_type]
    }

    crit_types = Enum.map(ability.traits["Damage"], & &1.damage_type_id)

    ability = Map.put(ability, :crit_tables, crit_types)

    if on_hit = weapon.traits["OnHit"] do
      # if on_hit.kind == "attack" do
      #   modifier = energy / 1000

      #   Enum.reduce(on_hit.traits["Damage"], ability, fn damage, ability ->
      #     damage =
      #       damage
      #       |> update_in([:min], &trunc(&1 * modifier))
      #       |> update_in([:max], &trunc(&1 * modifier))

      #     update_in(ability, [Access.key!(:traits), "Damage"], &[damage | &1])
      #   end)
      # else
      on_hit =
        Enum.map(on_hit, fn ability ->
          ability
          |> Map.put(:energy, 0)
          |> Map.put(:reaction_energy, 200)
          |> Map.put(:mana, 0)
          |> Map.put(:on_hit?, true)
          |> Map.put(:difficulty, nil)
        end)

      ability
      |> put_in([Access.key!(:traits), "OnHit"], on_hit)
      |> put_in([Access.key!(:traits), "OnHit%"], weapon.traits["OnHit%"])

      # end
    else
      ability
    end
  end

  def shield(%Character{} = character) do
    character.equipment
    |> Enum.filter(&(&1.type == "Shield"))
    |> case do
      [] ->
        nil

      list ->
        Enum.random(list)
    end
  end

  def sign_in(email, password) do
    player = Repo.get_by(Character, email: email)
    sign_in?(player, password) && player
  end

  def sign_in?(%Character{password: stored_hash}, password) do
    checkpw(password, stored_hash)
  end

  def sign_in?(nil, _password) do
    dummy_checkpw()
  end

  def decrement_highest_attribute(%Character{} = character) do
    {attribute, level} =
      Enum.max_by(character.attribute_levels, fn {_attribute, level} -> level end)

    if level > 1 do
      modifier = (100 + character.race.race.exp_modifier) / 100

      exp = Level.exp_at_level(level - 1, modifier)

      character =
        character
        |> update_in([:race], fn character_race ->
          character_race
          |> Ecto.Changeset.change(%{"#{attribute}_experience": exp})
          |> Repo.update!()
        end)
        |> set_attribute_levels()

      message =
        "<p><span class='yellow'>Your #{attribute} decreased to #{Map.get(character, attribute)}!</span></p>"

      Repo.insert!(%ChannelHistory{
        character_id: character.id,
        message: message
      })

      Character.send_chat(
        character,
        message
      )
    else
      character
    end
  end

  def trainable_experience(%Character{} = character) do
    used_experience =
      character.classes
      |> Enum.sort_by(fn c -> Repo.get(Class, c.class_id).exp_modifier end, &>=/2)
      |> Enum.reduce(0, fn character_class, used_experience ->
        level = character_class.level
        class = Repo.get(Class, character_class.class_id)

        modifier = class.exp_modifier / 100

        used_experience + Level.exp_at_level(level - 1, modifier)
      end)

    character.experience - used_experience
  end

  def add_attribute_experience(%Character{exp_buffer: buffer} = character, attribute, amount)
      when buffer >= amount do
    Character.pulse_score_attribute(character, attribute)

    attribute_level = character.attribute_levels[attribute]

    character =
      character
      |> Ecto.Changeset.change(%{
        exp_buffer: Map.get(character, :exp_buffer) - amount
      })
      |> Repo.update!()
      |> update_in([:race], fn character_race ->
        character_race
        |> Ecto.Changeset.change(%{
          "#{attribute}_experience":
            Map.get(character_race, :"#{attribute}_experience") + trunc(amount)
        })
        |> Repo.update!()
      end)

    modifier = (100 + character.race.race.exp_modifier) / 100

    new_attribute_level =
      character
      |> get_in([Access.key!(:race), Access.key!(:"#{attribute}_experience")])
      |> Level.level_at_exp(modifier)

    character =
      if new_attribute_level > attribute_level do
        character = Character.set_attribute_levels(character)

        message =
          "<p><span class='yellow'>Your #{attribute} increased to #{Map.get(character, attribute)}!</span></p>"

        Repo.insert!(%ChannelHistory{
          character_id: character.id,
          message: message
        })

        Character.send_chat(
          character,
          message
        )

        character
      else
        character
      end

    Character.update_attribute_bar(character, attribute)
    Character.update_exp_bar(character)

    character
  end

  def add_attribute_experience(%Character{} = character, _attribute, _amount), do: character

  def add_attribute_experience(%Character{} = character, %{} = attributes) do
    max_buffer = Character.max_exp_buffer(character)

    percent = min(1.0, character.exp_buffer / max_buffer) * 2

    exp = max(1, trunc(character.exp_buffer * 0.01) * percent)

    Enum.reduce(attributes, character, fn {attribute, percent}, character ->
      amount = max(1, trunc(exp * percent))

      Character.add_attribute_experience(character, attribute, amount)
    end)
  end

  def add_attribute_experience(%{} = character, %{} = _attributes), do: character

  def add_skill_experience(character, skill_name, amount \\ nil)
  def add_skill_experience(%Character{} = character, nil, _amount), do: character

  def add_skill_experience(%Character{} = character, skill_name, amount) do
    amount =
      if amount do
        amount
      else
        max_buffer = Character.max_exp_buffer(character)

        percent = min(1.0, character.exp_buffer / max_buffer) * 2

        exp = max(1, trunc(character.exp_buffer * 0.01) * percent)

        max(1, exp * character.skills[skill_name].exp_multiplier)
      end

    skill_level = character.skills[skill_name].level

    skill =
      character.skills[skill_name]
      |> Ecto.Changeset.change(%{
        experience: character.skills[skill_name].experience + trunc(amount)
      })
      |> Repo.update!()
      |> Skill.set_level()

    character = put_in(character.skills[skill_name], skill)

    new_skill_level = skill.level

    if new_skill_level > skill_level do
      message =
        "<p><span class='yellow'>Your #{skill_name} skill increased to #{new_skill_level}!</span></p>"

      Repo.insert!(%ChannelHistory{
        character_id: character.id,
        message: message
      })

      Character.send_chat(
        character,
        message
      )
    else
      character
    end
  end

  def drain_exp_buffer(%Character{exp_buffer: 0} = character), do: character

  def drain_exp_buffer(%Character{next_drain_at: drain_at} = character) do
    now = DateTime.utc_now() |> DateTime.to_unix(:millisecond) |> trunc

    if is_nil(drain_at) or drain_at < now do
      amount = Character.drain_rate(character)

      character
      |> put_in([:next_drain_at], now + :timer.seconds(10))
      |> update_in([:exp_buffer], &max(0, &1 - amount))
      |> add_character_experience(amount)
    else
      character
    end
  end

  def add_experience_to_buffer(character, exp, silent \\ false)

  def add_experience_to_buffer(%Character{} = character, exp, silent) when exp > 0 do
    unless silent, do: Mobile.send_scroll(character, "<p>You gain #{exp} experience.</p>")

    # 6 attributes, give 6 times the exp so stats level at approximately the same rate as classes
    # exp = exp * 6

    exp_buffer = character.exp_buffer + exp

    character =
      character
      |> Ecto.Changeset.change(%{
        exp_buffer: exp_buffer
      })
      |> Repo.update!()

    Statix.increment("exp_gained", exp, tags: ["character:#{String.downcase(character.name)}"])

    Character.update_exp_bar(character)
  end

  def add_experience_to_buffer(character, _exp, _silent), do: character

  def add_character_experience(%Character{} = character, exp) when exp > 0 do
    character =
      character
      |> Ecto.Changeset.change(%{
        experience: character.experience + exp
      })
      |> Repo.update!()

    Character.update_exp_bar(character)
  end

  def add_character_experience(%Character{} = character, _exp), do: character

  def prompt(%Character{level: level, hp: hp_percent, mana: mana_percent} = character) do
    max_hp = Mobile.max_hp_at_level(character, level)
    max_mana = Mobile.max_mana_at_level(character, level)
    hp = trunc(max_hp * hp_percent)
    mana = trunc(max_mana * mana_percent)

    if character.editing do
      "[HP=<span class='#{hp_prompt_color(hp_percent)}'>#{hp}</span>/MA=#{mana}] <span class='yellow'>*#{
        character.editing.name
      }*</span>:"
    else
      if max_mana > 0 do
        "[HP=<span class='#{hp_prompt_color(hp_percent)}'>#{hp}</span>/MA=#{mana}]:"
      else
        "[HP=<span class='#{hp_prompt_color(hp_percent)}'>#{hp}</span>]:"
      end
    end
  end

  def hp_prompt_color(hp_percent) when hp_percent > 0.5, do: "grey"
  def hp_prompt_color(hp_percent) when hp_percent > 0.2, do: "dark-red"
  def hp_prompt_color(_hp_percent), do: "red"

  def hp_at_level(%Character{} = character, level) do
    max_hp = Mobile.max_hp_at_level(character, level)

    trunc(max_hp * character.hp)
  end

  def mana_at_level(%Character{} = character, level) do
    max_mana = Mobile.max_mana_at_level(character, level)

    trunc(max_mana * character.mana)
  end

  def update_score(%Character{socket: socket} = character, room) do
    data = score_data(character, room)
    send(socket, {:update_score, data})
  end

  def show_talent_tree(%Character{socket: socket} = _character, _room) do
    send(socket, :show_talent_tree)
  end

  def update_energy_bar(%Character{socket: socket} = character, mobile) do
    percent = mobile.energy / mobile.max_energy

    send(
      socket,
      {:update_energy_bar,
       %{
         ref: mobile.ref,
         player: mobile.ref == character.ref,
         percentage: trunc(percent * 100),
         max_percent: 100
       }}
    )

    character
  end

  def update_mana_bar(%Character{socket: socket} = character, mobile, _room) do
    percent = mobile.mana

    send(
      socket,
      {:update_mana_bar,
       %{
         ref: mobile.ref,
         player: mobile.ref == character.ref,
         percentage: trunc(percent * 100),
         max_percent: 100
       }}
    )

    character
  end

  def update_mana_bar(%{} = character), do: character

  def update_hp_bar(%Character{socket: socket} = character, mobile, _room) do
    percent = mobile.hp

    send(
      socket,
      {:update_hp_bar,
       %{
         ref: mobile.ref,
         player: mobile.ref == character.ref,
         percentage: trunc(percent * 100),
         shield: Mobile.ability_value(mobile, "Bubble")
       }}
    )

    character
  end

  def pulse_score_attribute(%Character{socket: socket}, attribute) do
    send(socket, {:pulse_score_attribute, attribute})
  end

  def score_data(%Character{} = character, room) do
    effects =
      character.effects
      |> Map.values()
      |> Enum.filter(&Map.has_key?(&1, "StatusMessage"))
      |> Enum.map(& &1["StatusMessage"])
      |> Enum.group_by(& &1)
      |> Enum.map(fn {effect, list} ->
        if length(list) > 1 do
          effect <> " (x#{length(list)})"
        else
          effect
        end
      end)

    limbs =
      character.limbs
      |> Enum.reduce(%{}, fn {limb_name, limb}, limbs ->
        Map.put(
          limbs,
          limb_name,
          "#{trunc(limb.health * 100)}%"
        )
      end)

    %{
      name: character.name,
      race: character.race.race.name,
      combat: Character.combat_level(character),
      level: character.level,
      alignment: legal_status(character),
      perception: Mobile.perception_at_level(character, character.level, room),
      accuracy: Mobile.accuracy_at_level(character, character.level, room),
      crits: Mobile.crits_at_level(character, character.level),
      dodge: Mobile.dodge_at_level(character, character.level, room),
      stealth: Mobile.stealth_at_level(character, character.level),
      block: Mobile.block_at_level(character, character.level),
      parry: Mobile.parry_at_level(character, character.level),
      physical_resistance: Mobile.physical_resistance_at_level(character, character.level),
      magical_damage: Mobile.magical_penetration_at_level(character, character.level),
      magical_resistance: Mobile.magical_resistance_at_level(character, character.level),
      hp: hp_at_level(character, character.level),
      max_hp: Mobile.max_hp_at_level(character, character.level),
      mana: mana_at_level(character, character.level),
      max_mana: Mobile.max_mana_at_level(character, character.level),
      energy: character.energy,
      max_energy: character.max_energy,
      strength: Mobile.attribute_at_level(character, :strength, character.level),
      agility: Mobile.attribute_at_level(character, :agility, character.level),
      intellect: Mobile.attribute_at_level(character, :intellect, character.level),
      willpower: Mobile.attribute_at_level(character, :willpower, character.level),
      health: Mobile.attribute_at_level(character, :health, character.level),
      charm: Mobile.attribute_at_level(character, :charm, character.level),
      effects: effects,
      limbs: limbs,
      round_length_in_ms: 5000,
      spellcasting:
        Mobile.spellcasting_at_level(character, character.level, %{
          attributes: ["intellect", "willpower"]
        })
    }
  end

  def weapon_damage(weapon_speed, target_damage_per_round, level) do
    # 50% encumbrance
    character = %Character{
      level: level,
      strength: 50,
      agility: 49 + level,
      inventory: [%{weight: 1200}]
    }

    weapon = %Item{speed: weapon_speed}

    energy = max(200, energy_per_swing(character, weapon))

    avg = target_damage_per_round * (energy / 1000)

    %{min_damage: avg * 0.75, max_damage: avg * 1.25}
  end

  def combat_level(%Character{} = character) do
    {total_combat, total_level} =
      character.effects
      |> Map.values()
      |> Enum.reduce({0, 0}, fn
        %{"ClassCombatLevel" => combat, "ClassLevel" => level}, {total_combat, total_level} ->
          {combat + total_combat, level + total_level}

        _, {total_combat, total_level} ->
          {total_combat, total_level}
      end)

    if total_level > 0 do
      class_combat_level = Float.round(total_combat / total_level, 2)

      class_combat_level + Mobile.ability_value(character, "CombatLevel")
    else
      Mobile.ability_value(character, "CombatLevel")
    end
  end

  def energy_per_swing(character, weapon \\ nil) do
    weapon = weapon || Character.weapon(character)
    encumbrance = Character.encumbrance(character)
    max_encumbrance = Character.max_encumbrance(character)
    agility = Mobile.attribute_at_level(character, :agility, character.level)

    combat_level = Character.combat_level(character)

    cost =
      weapon.speed * 1000 /
        ((character.level * (combat_level + 2) + 45) * (agility + 150) *
           1500 /
           9000.0)

    energy =
      trunc(
        cost * (Float.floor(Float.floor(encumbrance / max_encumbrance * 100) / 2.0) + 75) / 100.0
      )

    min(energy, 1000)
  end

  def send_chat(%Character{socket: socket} = character, html) do
    send(socket, {:chat, html})
    character
  end

  def drain_rate(character) do
    level = max(1, character.level)

    exp_to_level = Level.exp_at_level(level) - Level.exp_at_level(level - 1)

    target_time = 20 * level

    trunc(max(Float.round(exp_to_level / (target_time * 60) * 10), 10.0))
  end

  def max_exp_buffer(character) do
    rate = drain_rate(character)

    trunc(rate / 10 * 60 * 60)
  end

  def update_exp_bar(%Character{socket: socket} = character) do
    max_buffer = Character.max_exp_buffer(character)

    percent = min(100, character.exp_buffer / max_buffer * 100)

    send(
      socket,
      {:update_exp_bar,
       %{
         percentage: percent
       }}
    )

    character
  end

  def update_attribute_bar(%Character{socket: socket} = character, attribute) do
    level = character.attribute_levels[attribute]

    exp = get_in(character, [Access.key!(:race), Access.key!(:"#{attribute}_experience")])
    current = Level.exp_at_level(level, 1.0)
    to_level = Level.exp_at_level(level + 1, 1.0)

    percent = ((exp - current) / (to_level - current) * 100) |> trunc

    send(
      socket,
      {:update_attribute_bar,
       %{
         attribute: to_string(attribute),
         percentage: percent
       }}
    )

    character
  end

  def alter_evil_points(character, points) do
    initial_legal_status = Character.legal_status(character)

    change =
      if points > 0 do
        Mobile.send_scroll(
          character,
          "<p><span class='dark-grey'>A dark cloud passes over you</span></p>"
        )

        %{
          evil_points: min(300.0, character.evil_points + points),
          last_evil_action_at: DateTime.utc_now()
        }
      else
        %{evil_points: max(-220.0, character.evil_points + points)}
      end

    character =
      character
      |> Ecto.Changeset.cast(change, ~w(evil_points last_evil_action_at)a)
      |> Repo.update!()

    Directory.add_character(%{
      name: character.name,
      evil_points: character.evil_points,
      room: character.room_id,
      ref: character.ref,
      title: character.title
    })

    legal_status = Character.legal_status(character)

    if legal_status != initial_legal_status do
      color = ApathyDrive.Commands.Who.color(legal_status)

      status = "<span class='#{color}'>#{legal_status}</span>"

      Mobile.send_scroll(
        character,
        "<p>Your legal status has changed to #{status}.</p>"
      )

      Character.load_abilities(character)
    else
      character
    end
  end

  defimpl ApathyDrive.Mobile, for: Character do
    def ability_value(character, ability) do
      merge_by = Trait.merge_by(ability)
      character_value = Systems.Effect.effect_bonus(character, ability, merge_by)

      Enum.reduce(character.equipment, character_value, fn item, value ->
        bonus = Systems.Effect.effect_bonus(item, ability, merge_by)

        case merge_by do
          "add" ->
            (value || 0) + (bonus || 0)

          "multiply" ->
            (value || 1) * (bonus || 1)

          "list" ->
            [bonus | List.wrap(value)]
            |> List.flatten()
            |> Enum.reject(&is_nil/1)

          "replace" ->
            bonus || value
        end
      end)
    end

    def accuracy_at_level(character, _level, _room) do
      agility = Mobile.attribute_at_level(character, :agility, character.level)
      charm = Mobile.attribute_at_level(character, :charm, character.level)

      agi = agility + charm / 10
      modifier = ability_value(character, "Accuracy")
      trunc(agi * (1 + modifier / 100))
    end

    def attribute_at_level(%Character{} = character, attribute, _level) do
      Map.get(character, attribute) +
        ability_value(character, attribute |> to_string |> String.capitalize())
    end

    def attack_ability(character, riposte) do
      punch = %Item{
        type: "Weapon",
        name: "fist",
        hit_verbs: [["punch", "punches"]],
        miss_verbs: ["throw a punch", "throws a punch"],
        min_damage: 2,
        max_damage: 7,
        speed: 1150
      }

      punch =
        if damage = Mobile.ability_value(character, "WeaponDamage") do
          put_in(punch.effects[1], %{"WeaponDamage" => damage})
        else
          punch
        end

      weapon = Character.weapon(character) || punch

      Character.ability_for_weapon(character, weapon, riposte)
    end

    def auto_attack_target(%Character{attack_target: target} = _character, room) do
      if room.mobiles[target], do: target
    end

    def confused(%Character{effects: effects} = character, %Room{} = room) do
      effects
      |> Map.values()
      |> Enum.find(fn effect ->
        Map.has_key?(effect, "Confusion") && effect["Confusion"] >= :rand.uniform(100)
      end)
      |> confused(character, room)
    end

    def confused(nil, %Character{}, %Room{}), do: false

    def confused(
          %{"ConfusionMessage" => message} = effect,
          %Character{} = character,
          %Room{} = room
        ) do
      Mobile.send_scroll(character, "<p>#{message}</p>")

      if effect["ConfusionSpectatorMessage"],
        do:
          Room.send_scroll(
            room,
            "<p>#{Text.interpolate(effect["ConfusionSpectatorMessage"], %{"user" => character})}</p>",
            [character]
          )

      true
    end

    def confused(%{}, %Character{} = character, %Room{} = room) do
      send_scroll(character, "<p><span class='cyan'>You fumble in confusion!</span></p>")

      Room.send_scroll(
        room,
        "<p><span class='cyan'>#{
          Text.interpolate("{{user}} fumbles in confusion!</span></p>", %{"user" => character})
        }</span></p>",
        [character]
      )

      true
    end

    def color(%Character{} = character) do
      case Character.alignment(character) do
        "good" ->
          "grey"

        "neutral" ->
          "dark-cyan"

        "evil" ->
          "magenta"
      end
    end

    def colored_name(%Character{name: name} = character) do
      "<span class='#{color(character)}'>#{name}</span>"
    end

    def crits_at_level(character, level) do
      intellect = attribute_at_level(character, :intellect, level)
      charm = attribute_at_level(character, :charm, level)

      base = div(intellect * 3 + charm, 6) + level * 2

      trunc(base / (250 + base) * 100) + ability_value(character, "Crits")
    end

    def description(%Character{} = character, %Character{} = observer) do
      character_level = character.level
      observer_level = observer.level

      descriptions =
        [
          strength: [
            "puny",
            "weak",
            "slightly built",
            "moderately built",
            "well built",
            "muscular",
            "powerfully built",
            "heroically proportioned",
            "Herculean",
            "physically Godlike"
          ],
          health: [
            "frail",
            "thin",
            "healthy",
            "stout",
            "solid",
            "massive",
            "gigantic",
            "colossal"
          ],
          agility: [
            "slowly",
            "clumsily",
            "slugishly",
            "cautiously",
            "gracefully",
            "very swiftly",
            "with uncanny speed",
            "with catlike agility",
            "blindingly fast"
          ],
          charm: [
            "openly hostile and quite revolting.",
            "hostile and unappealing.",
            "quite unfriendly and aloof.",
            "likeable in an unassuming sort of way.",
            "quite attractive and pleasant to be around.",
            "charismatic and outgoing. You can't help but like {{target:him/her/them}}.",
            "extremely likeable, and fairly radiates charisma.",
            "incredibly charismatic. You are almost overpowered by {{target:his/her/their}} strong personality.",
            "overwhelmingly charismatic. You almost drop to your knees in wonder at the sight of {{target:him/her/them}}!"
          ],
          intellect: [
            "utterly moronic",
            "quite stupid",
            "slightly dull",
            "intelligent",
            "bright",
            "extremely clever",
            "brilliant",
            "a genius",
            "all-knowing"
          ],
          willpower: [
            "selfish and hot-tempered",
            "sullen and impulsive",
            "a little naive",
            "looks fairly knowledgeable",
            "looks quite experienced and wise",
            "has a worldly air about {{target:him/her/them}}",
            "seems to possess a wisdom beyond {{target:his/her/their}} years",
            "seem to be in an enlightened state of mind",
            "looks like {{target:he is/she is/they are}} one with the Gods"
          ]
        ]
        |> Enum.reduce(%{}, fn {stat, stat_descriptions}, descriptions ->
          character_stat = Mobile.attribute_at_level(character, stat, character_level)
          observer_stat = Mobile.attribute_at_level(observer, stat, observer_level)

          if character_stat < observer_stat do
            index =
              trunc((character_stat - observer_stat) / 2) + div(length(stat_descriptions), 2)

            Map.put(
              descriptions,
              stat,
              Enum.at(stat_descriptions, index, Enum.at(stat_descriptions, 0))
            )
          else
            index =
              trunc((character_stat - observer_stat) / 2) + div(length(stat_descriptions), 2)

            Map.put(
              descriptions,
              stat,
              Enum.at(stat_descriptions, index, Enum.at(stat_descriptions, -1))
            )
          end
        end)

      "#{character.name} is a #{descriptions[:health]}, #{descriptions[:strength]} #{
        character.race.race.name
      }. {{target:He moves/She moves/They move}} #{descriptions[:agility]}, and {{target:is/is/are}} #{
        descriptions[:charm]
      } #{character.name} appears to be #{descriptions[:intellect]} and #{
        descriptions[:willpower]
      }."
    end

    def detected?(character, sneaker, room) do
      perception = Mobile.perception_at_level(character, character.level, room)
      stealth = Mobile.stealth_at_level(sneaker, sneaker.level)

      :rand.uniform(100) >= stealth - div(perception, 3)
    end

    def die?(character) do
      max_hp = Mobile.max_hp_at_level(character, character.level)

      missing_fatal_limb =
        character.limbs
        |> Map.values()
        |> Enum.any?(&(&1.fatal && &1.health <= 0))

      max_hp <= 0 or character.hp <= 0 or missing_fatal_limb
    end

    def die(character, room) do
      if Mobile.has_ability?(character, "DeusExMachina") do
        room =
          Room.update_mobile(room, character.ref, fn room, character ->
            Room.send_scroll(
              room,
              "<p><span class='red'>#{character.name} has died.</span></p>",
              [
                character
              ]
            )

            Room.send_scroll(
              room,
              "<p><span class='blue'>#{character.name} has been resurrected!</span></p>",
              [character]
            )

            character =
              character
              |> Mobile.send_scroll("<p><span class='red'>You have died.</span></p>")
              |> Mobile.send_scroll("<p><span class='blue'>You have been resurrected!</span></p>")
              |> Map.put(:hp, 1.0)
              |> Map.put(:mana, 1.0)
              |> Map.put(:energy, character.max_energy)
              |> Systems.Effect.remove_all_stacks(:holy_mission_damage)

            character =
              character.effects
              |> Enum.filter(fn {_key, effect} ->
                Map.has_key?(effect, "DeusExMachina")
              end)
              |> Enum.map(fn {key, _effect} -> key end)
              |> Enum.reduce(
                character,
                &Systems.Effect.remove(&2, &1,
                  fire_after_cast: true,
                  show_expiration_message: true
                )
              )

            character.limbs
            |> Enum.reduce(character, fn {limb_name, _limb}, character ->
              character
              |> Systems.Effect.remove_oldest_stack({:severed, limb_name})
              |> Systems.Effect.remove_oldest_stack({:crippled, limb_name})
            end)
            |> Ecto.Changeset.change(%{
              missing_limbs: []
            })
            |> Repo.update!()
            |> Character.load_limbs()
          end)

        Room.update_moblist(room)
        room
      else
        character =
          character
          |> Mobile.send_scroll("<p><span class='red'>You have died.</span></p>")
          |> Map.put(:race_id, character.death_race_id || character.race_id)
          |> Map.put(:hp, 1.0)
          |> Map.put(:mana, 1.0)
          |> Map.put(:energy, character.max_energy)
          |> Map.put(:attack_target, nil)
          |> Map.put(:attack_roam, false)
          |> update_in([:effects], fn effects ->
            effects
            |> Enum.filter(fn {_key, effect} -> effect["stack_key"] == "class" end)
            |> Enum.into(%{})
          end)
          |> Map.put(:timers, %{})
          |> Character.load_race()
          |> Ecto.Changeset.change(%{
            missing_limbs: []
          })
          |> Repo.update!()
          |> Character.decrement_highest_attribute()
          |> Character.load_limbs()
          |> Character.load_traits()
          |> Character.set_attribute_levels()
          |> Character.add_equipped_items_effects()
          |> Character.load_abilities()
          |> Mobile.update_prompt(room)
          |> TimerManager.send_after(
            {:reduce_evil_points, :timer.seconds(60), {:reduce_evil_points, character.ref}}
          )

        Room.start_room_id()
        |> RoomServer.find()
        |> RoomServer.mobile_entered(character)

        room =
          character
          |> Character.companion(room)
          |> Companion.dismiss(room)

        room =
          put_in(room.mobiles, Map.delete(room.mobiles, character.ref))
          |> Room.send_scroll("<p><span class='red'>#{character.name} has died.</span></p>")

        Room.update_moblist(room)
        room
      end
    end

    def dodge_at_level(character, level, _room) do
      agi = attribute_at_level(character, :agility, level)
      cha = attribute_at_level(character, :charm, level)
      base = agi + cha / 10

      trunc(base + ability_value(character, "Dodge"))
    end

    def block_at_level(character, level) do
      str = attribute_at_level(character, :strength, level)
      cha = attribute_at_level(character, :charm, level)
      base = str + cha / 10
      trunc(base + ability_value(character, "Block"))
    end

    def parry_at_level(character, _level) do
      str = Mobile.attribute_at_level(character, :strength, character.level)
      agi = Mobile.attribute_at_level(character, :agility, character.level)
      cha = Mobile.attribute_at_level(character, :charm, character.level)
      base = (str + agi) / 2 + cha / 10
      trunc(base + Mobile.ability_value(character, "Parry"))
    end

    def enough_mana_for_ability?(character, %Ability{mana: cost} = _ability) do
      mana = Character.mana_at_level(character, character.level)

      mana >= cost
    end

    def evil_points(character, %Character{} = attacker) do
      cond do
        character.ref == attacker.ref ->
          0

        Ability.retaliate?(character, attacker) ->
          # attacker has already received evil points for attacking character
          0

        Ability.retaliate?(attacker, character) ->
          # character has attacked the attacker, it's not evil to fight back
          0

        :else ->
          character
          |> Character.legal_status()
          |> case do
            "Saint" ->
              60

            "Good" ->
              40

            "Neutral" ->
              20

            _ ->
              0
          end
      end
    end

    # only characters can receive evil points
    def evil_points(_character, %{} = _attacker), do: 0

    def enter_message(%Character{name: name}) do
      "<p><span class='yellow'>#{name}</span><span class='dark-green'> walks in from {{direction}}.</span></p>"
    end

    def exit_message(%Character{name: name}) do
      "<p><span class='yellow'>#{name}</span><span class='dark-green'> walks off {{direction}}.</span></p>"
    end

    def has_ability?(%Character{} = character, ability_name) do
      character.effects
      |> Map.values()
      |> Enum.map(&Map.keys/1)
      |> List.flatten()
      |> Enum.member?(ability_name)
    end

    def hp_regen_per_round(%Character{hp: hp} = character) when hp >= 0 do
      if Enum.any?(character.limbs, fn {_name, limb} -> limb.health < 0 end) do
        0.0001
      else
        base_hp_regen =
          (character.level + 30) * attribute_at_level(character, :health, character.level) / 500.0 *
            5000 / 30_000

        modified_hp_regen = base_hp_regen * (1 + ability_value(character, "HPRegen") / 100)

        max_hp = max_hp_at_level(character, character.level)

        modified_hp_regen / max_hp
      end
    end

    def hp_regen_per_round(%Character{hp: _hp} = _character), do: 0

    def mana_regen_per_round(%Character{} = character) do
      max_mana = max_mana_at_level(character, character.level)

      spellcasting =
        Mobile.spellcasting_at_level(character, character.level, %{attributes: ["willpower"]})

      base_mana_regen =
        (character.level + 20) * spellcasting *
          (div(ability_value(character, "ManaPerLevel"), 2) + 2) / 1650.0 * 5000 / 30_000

      modified_mana_regen = base_mana_regen * (1 + ability_value(character, "ManaRegen") / 100)

      if max_mana > 0 do
        modified_mana_regen / max_mana
      else
        0
      end
    end

    def heartbeat(%Character{} = character, %Room{} = room) do
      initial_hp = character.hp
      initial_max_hp = Mobile.max_hp_at_level(character, character.level)

      room =
        Room.update_mobile(room, character.ref, fn room, character ->
          hp = Regeneration.hp_since_last_tick(room, character)

          room =
            room
            |> Regeneration.heal_limbs(character.ref, hp)
            |> Regeneration.balance_limbs(character.ref)
            |> Ability.unbalance(character.ref)

          character = room.mobiles[character.ref]

          if character do
            character
            |> Regeneration.regenerate(room)
            |> Character.drain_exp_buffer()
            |> RoomServer.execute_casting_ability(room)
          else
            room
          end
        end)
        |> ApathyDrive.Aggression.react(character.ref)
        |> AI.think(character.ref)

      if character = room.mobiles[character.ref] do
        if Mobile.die?(character) do
          Mobile.die(character, room)
        else
          cond do
            initial_hp < 0 and character.hp >= 0 ->
              Room.send_scroll(
                room,
                "<p>#{Mobile.colored_name(character)} stands up.</p>",
                [
                  character
                ]
              )

              Mobile.send_scroll(character, "<p>You stand up.</p>")

              Room.update_hp_bar(room, character.ref)

            initial_max_hp != Mobile.max_hp_at_level(character, character.level) ->
              Room.update_hp_bar(room, character.ref)

            :else ->
              :noop
          end

          room
        end
      else
        room
      end
    end

    def exhausted(%{energy: energy} = character) do
      required_energy = ApathyDrive.Commands.Move.energy_cost(character)

      if energy < required_energy do
        true
      else
        false
      end
    end

    def held(%{effects: effects} = mobile) do
      effects
      |> Map.values()
      |> Enum.find(fn effect ->
        Map.has_key?(effect, "Root")
      end)
      |> held(mobile)
    end

    def held(nil, %{}), do: false

    def held(%{"StatusMessage" => message}, %{} = mobile) do
      send_scroll(mobile, "<p>#{message}</p>")
      true
    end

    def hp_description(%Character{hp: hp}) when hp >= 1.0, do: "unwounded"
    def hp_description(%Character{hp: hp}) when hp >= 0.9, do: "slightly wounded"
    def hp_description(%Character{hp: hp}) when hp >= 0.6, do: "moderately wounded"
    def hp_description(%Character{hp: hp}) when hp >= 0.4, do: "heavily wounded"
    def hp_description(%Character{hp: hp}) when hp >= 0.2, do: "severely wounded"
    def hp_description(%Character{hp: hp}) when hp >= 0.1, do: "critically wounded"
    def hp_description(%Character{hp: _hp}), do: "very critically wounded"

    def magical_penetration_at_level(character, level) do
      attribute = attribute_at_level(character, :intellect, level) - 50

      penetration = attribute + ability_value(character, "MagicalPenetration")

      max(0, penetration)
    end

    def magical_resistance_at_level(character, level) do
      willpower = attribute_at_level(character, :willpower, level)

      mr = ability_value(character, "MR")

      trunc(max(max(willpower - 50, 0) + mr, 0))
    end

    def max_hp_at_level(mobile, level) do
      health = attribute_at_level(mobile, :health, level)

      base = health / 2
      hp_per_level = ability_value(mobile, "HPPerLevel") * level
      bonus = (health - 50) * level / 16

      max_hp_percent = ability_value(mobile, "MaxHP%")

      modifier = if max_hp_percent, do: max_hp_percent, else: 1.0

      max(1, trunc((base + hp_per_level + bonus + ability_value(mobile, "MaxHP")) * modifier))
    end

    def max_mana_at_level(mobile, level) do
      mana_per_level = ability_value(mobile, "ManaPerLevel")

      bonus = ability_value(mobile, "MaxMana")

      base_mana = 6

      mana_per_level * (level - 1) + base_mana + bonus
    end

    def party_refs(character, room) do
      Party.refs(room, character)
    end

    def perception_at_level(character, level, room) do
      intellect = attribute_at_level(character, :intellect, level)
      charm = attribute_at_level(character, :charm, level)

      base = div(intellect * 3 + charm, 6) + level * 2

      base = base + ability_value(character, "Perception")

      light_modifier =
        room
        |> Room.light()
        |> ApathyDrive.Commands.Look.light_for_character(character)
        |> Room.light_modifier()

      trunc(base * (1 - light_modifier / 100))
    end

    def physical_penetration_at_level(character, level) do
      attribute = attribute_at_level(character, :strength, level) - 50

      penetration = attribute + ability_value(character, "PhysicalPenetration")

      max(0, penetration)
    end

    def physical_resistance_at_level(character, level) do
      strength = attribute_at_level(character, :strength, level)
      ac = ability_value(character, "AC")

      trunc(max(max(strength - 50, 0) + ac, 0))
    end

    def power_at_level(%Character{} = character, level) do
      [:strength, :agility, :intellect, :willpower, :health, :charm]
      |> Enum.reduce(0, &(&2 + Mobile.attribute_at_level(character, &1, level)))
    end

    def send_scroll(%Character{socket: socket} = character, html) do
      send(socket, {:scroll, html})
      character
    end

    def set_room_id(%Character{socket: socket, monitor_ref: monitor_ref} = character, room_id) do
      Process.demonitor(monitor_ref)

      send(
        character.socket,
        {:update_character,
         %{
           room_id: room_id,
           power: Mobile.power_at_level(character, character.level),
           level: character.level
         }}
      )

      Directory.add_character(%{
        name: character.name,
        evil_points: character.evil_points,
        room: room_id,
        ref: character.ref,
        title: character.title
      })

      character
      |> Map.put(:last_room_id, character.room_id)
      |> Map.put(:room_id, room_id)
      |> Map.put(:monitor_ref, Process.monitor(socket))
      |> Repo.save!()
    end

    def shift_hp(character, percentage) do
      update_in(character.hp, &min(1.0, &1 + percentage))
    end

    def silenced(%Character{effects: effects} = character, %Room{} = room) do
      effects
      |> Map.values()
      |> Enum.find(fn effect ->
        Map.has_key?(effect, "Silence")
      end)
      |> silenced(character, room)
    end

    def silenced(nil, %Character{}, %Room{}), do: false

    def silenced(%{}, %Character{} = character, %Room{}) do
      Mobile.send_scroll(character, "<p><span class='cyan'>You are silenced!</span></p>")
      true
    end

    def spellcasting_at_level(character, level, ability) do
      attribute_value =
        (ability.attributes || [])
        |> Enum.map(&Mobile.attribute_at_level(character, String.to_atom(&1), character.level))
        |> Room.average()

      magic_level = div(Mobile.max_mana_at_level(character, level) - 6, 6)

      sc = trunc(attribute_value * 2 / 3) + magic_level * 5

      sc = sc + level * 2

      trunc((sc + ability_value(character, "Spellcasting")) * character.limbs["head"].health)
    end

    def stealth_at_level(character, level) do
      agility = attribute_at_level(character, :agility, level)
      charm = attribute_at_level(character, :charm, level)

      base = div(agility * 3 + charm, 6) + level * 2

      modifier =
        cond do
          character.race.race.stealth ->
            0.4

          :else ->
            0
        end

      max(0, trunc(base * modifier) + ability_value(character, "Stealth"))
    end

    def subtract_mana(character, %{mana: 0} = _ability), do: character

    def subtract_mana(character, %Ability{mana: cost}) do
      percentage = cost / Mobile.max_mana_at_level(character, character.level)

      character
      |> update_in([Access.key!(:mana)], &max(0, &1 - percentage))
    end

    def subtract_energy(character, ability) do
      character = update_in(character.energy, &(&1 - ability.energy))

      attributes = ability.attributes || %{}

      Enum.reduce(attributes, character, fn attribute, character ->
        Character.add_attribute_experience(character, %{
          String.to_atom(attribute) => 1 / length(attributes)
        })
      end)
    end

    def tracking_at_level(character, level, room) do
      perception = perception_at_level(character, level, room)
      modifier = ability_value(character, "Tracking")
      perception * (modifier / 100)
    end

    def update_prompt(%Character{socket: socket} = character, room) do
      Room.update_hp_bar(room, character.ref)
      Room.update_mana_bar(room, character.ref)
      Room.update_energy_bar(room, character.ref)
      send(socket, {:update_prompt, Character.prompt(character)})
      character
    end
  end
end
