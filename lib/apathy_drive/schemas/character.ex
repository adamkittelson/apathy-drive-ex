defmodule ApathyDrive.Character do
  use Ecto.Schema
  use ApathyDriveWeb, :model

  alias ApathyDrive.{
    Ability,
    AbilityAttribute,
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
    ClassSkill,
    Currency,
    Directory,
    ElementalLores,
    Enchantment,
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
    field(:race_id, :integer)
    field(:pity_modifier, :integer, default: 0)
    field(:auto_heal, :boolean)
    field(:auto_bless, :boolean)
    field(:auto_curse, :boolean)
    field(:auto_nuke, :boolean)
    field(:auto_roam, :boolean)
    field(:auto_rest, :boolean)
    field(:auto_sneak, :boolean)
    field(:auto_flee, :boolean)
    field(:auto_attack, :boolean)
    field(:auto_pet_casting, :boolean, default: true)
    field(:evil_points, :float)
    field(:missing_limbs, {:array, :string}, default: [])
    field(:attack_color, :string, default: "red")
    field(:target_color, :string, default: "red")
    field(:spectator_color, :string, default: "red")
    field(:lore_name, :string)
    field(:evil_points_last_reduced_at, :utc_datetime_usec)
    field(:exp_buffer_last_drained_at, :utc_datetime_usec)

    field(:max_exp_buffer, :any, virtual: true)
    field(:last_auto_attack_at, :any, virtual: true)
    field(:resting, :boolean, virtual: true, default: false)
    field(:enchantment, :any, virtual: true)
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
    classes =
      CharacterClass
      |> Ecto.Query.order_by(desc: :experience)
      |> Ecto.Query.preload([:class, :character])
      |> ApathyDrive.Repo.all()

    classes
    |> Enum.map(
      &%{
        character: &1.character,
        class: &1.class.name,
        exp: trunc(&1.experience || 0)
      }
    )
    |> Enum.group_by(& &1.character)
    |> Enum.map(fn {_character, list} ->
      Enum.max_by(list, & &1.exp)
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

  def set_title(%Character{} = character) do
    Map.put(character, :title, Title.for_character(character))
  end

  def set_lore(%Character{} = character) do
    lore = ElementalLores.lore(character.lore_name)
    Map.put(character, :lore, lore)
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
    |> put_change(:email, String.downcase(to_string(params["email"])))
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
      character.skills
      |> Enum.map(fn {_name, %{skill_id: id, level: level}} ->
        ApathyDrive.SkillAbility.abilities_at_level(id, level)
      end)
      |> List.flatten()

    granted_abilities =
      character
      |> Mobile.ability_value("Grant")
      |> List.flatten()
      |> Enum.uniq()
      |> Enum.map(fn ability_id ->
        ability =
          ability_id
          |> Ability.find()
          |> Map.put(:attributes, AbilityAttribute.load_attributes(ability_id))

        %{ability: ability}
      end)

    base_class_abilities = Enum.filter(class_abilities, &(&1.ability.kind == "base-class"))

    character = set_class_traits(character, base_class_abilities)

    class_abilities = Enum.reject(class_abilities, &(&1.ability.kind == "base-class"))

    character =
      (class_abilities ++ skill_abilities ++ granted_abilities)
      |> Enum.reduce(character, fn
        %{ability: %Ability{id: id, kind: "passive"}}, character ->
          effect = AbilityTrait.load_traits(id)

          effect =
            effect
            |> Map.put("stack_count", effect["StackCount"] || 1)
            |> Map.put("stack_key", effect["StackKey"] || id)
            |> Map.delete("StatusMessage")
            |> Map.delete("RemoveMessage")

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
                update_in(ability.traits, &Map.put(&1, "Damage", damage))
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
      abilities = Systems.Effect.effect_bonus(item, "Grant")

      if Enum.any?(abilities) do
        [ability] = abilities

        update_in(character.abilities, fn abilities ->
          Map.put(abilities, ability.command, ability)
        end)
      else
        character
      end
    end)
  end

  def set_class_traits(character, []), do: character

  def set_class_traits(character, base_class_abilities) do
    traits =
      base_class_abilities
      |> Enum.map(fn %{ability: ability} ->
        ability.id
        |> AbilityTrait.load_traits()
        |> Map.put("Level", ability.level)
      end)

    max_level =
      traits
      |> Enum.map(& &1["Level"])
      |> Enum.max()

    max_hp =
      Enum.reduce(1..max_level, 0, fn level, max_hp ->
        hp =
          traits
          |> Enum.filter(&(&1["Level"] >= level))
          |> Enum.map(& &1["MaxHP"])
          |> Enum.reject(&is_nil/1)
          |> Enum.max(fn -> 0 end)

        hp + max_hp
      end)

    elemental =
      Enum.reduce(1..max_level, 0, fn level, total ->
        elemental =
          traits
          |> Enum.filter(&(&1["Level"] >= level))
          |> Enum.map(& &1["Elemental"])
          |> Enum.reject(&is_nil/1)
          |> Enum.max(fn -> 0 end)

        total + elemental
      end)

    max_mana =
      Enum.reduce(1..max_level, 0, fn level, max_mana ->
        mana =
          traits
          |> Enum.filter(&(&1["Level"] >= level))
          |> Enum.map(& &1["MaxMana"])
          |> Enum.reject(&is_nil/1)
          |> Enum.max(fn -> 0 end)

        mana + max_mana
      end)

    combat_level =
      Enum.reduce(1..max_level, 0, fn level, combat_level ->
        combat =
          traits
          |> Enum.filter(&(&1["Level"] >= level))
          |> Enum.map(& &1["ClassCombatLevel"])
          |> Enum.reject(&is_nil/1)
          |> Enum.max(fn -> 0 end)

        combat + combat_level
      end)

    combat_level = combat_level / max_level

    effect = %{
      "stack_key" => "class",
      "stack_count" => 1,
      "MaxHP" => max_hp,
      "MaxMana" => max_mana,
      "CombatLevel" => combat_level,
      "Elemental" => elemental
    }

    Systems.Effect.add(character, effect)
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
    character = put_in(character.skills, %{})

    character =
      character.classes
      |> Enum.reduce(character, fn %{class_id: id, level: level}, character ->
        id
        |> ClassSkill.load_skills()
        |> Enum.reduce(character, fn %{name: name} = skill, character ->
          character
          |> update_in([:skills], &Map.put_new(&1, name, Map.put(skill, :level, 0)))
          |> update_in([:skills, name, :level], &min(max(&1, level), character.level))
        end)
      end)

    Skill
    |> Ecto.Query.where([s], s.universal == true)
    |> Repo.all()
    |> Enum.reduce(character, fn skill, character ->
      case Repo.get_by(CharacterSkill, character_id: character.id, skill_id: skill.id) do
        nil ->
          character_skill =
            %CharacterSkill{
              character_id: character.id,
              skill_id: skill.id,
              exp_multiplier: skill.exp_multiplier,
              experience: 0
            }
            |> Repo.insert!()

          character_skill = Skill.set_level(character_skill)

          put_in(character.skills[skill.name], character_skill)

        %CharacterSkill{} = character_skill ->
          character_skill =
            character_skill
            |> Map.put(:exp_multiplier, skill.exp_multiplier)
            |> Skill.set_level()

          put_in(character.skills[skill.name], character_skill)
      end
    end)
  end

  def load_classes(%Character{} = character) do
    classes =
      character
      |> Ecto.assoc(:character_classes)
      |> Repo.all()
      |> Repo.preload([:class])

    character
    |> Map.put(:classes, classes)
    |> set_level()
  end

  def set_level(%Character{classes: []} = character) do
    Map.put(character, :level, 1)
  end

  def set_level(%Character{classes: classes} = character) do
    level =
      classes
      |> Enum.map(& &1.level)
      |> Enum.max()

    Map.put(character, :level, max(level, 1))
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

    character =
      character
      |> Map.put(:inventory, Enum.reject(items, & &1.equipped))
      |> Map.put(:equipment, Enum.filter(items, & &1.equipped))
      |> assign_limbs_to_equipment()
      |> add_equipped_items_effects()
      |> load_abilities()
      |> TimerManager.send_after(
        {:use_light_source, :timer.seconds(30), {:use_light_source, character.ref}}
      )

    character
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

    character =
      Enum.reduce(character.equipment, character, fn item, updated_character ->
        add_equipped_item_effects(updated_character, item)
      end)

    character
  end

  def add_equipped_item_effects(%Character{} = character, item) do
    effect =
      item.effects
      |> Map.values()
      |> Enum.reduce(%{}, &Trait.merge_traits(&2, &1))
      |> Ability.duration_traits()
      |> Ability.process_duration_traits(character, character, nil)
      |> Map.put("stack_key", "item-#{item.instance_id}")
      |> Map.put("stack_count", 1)

    effect =
      if "DamageShield" in Map.keys(effect) do
        Ability.process_duration_trait({"Damage", effect["Damage"]}, effect, nil, nil, nil)
      else
        effect
      end

    skill = Item.skill_for_character(character, item)

    modifier =
      if skill == 0 do
        0.1
      else
        skill / character.level
      end

    effect =
      effect
      |> Map.put_new("AC", 0)
      |> update_in(["AC"], &trunc(&1 * modifier))
      |> Map.put_new("MR", 0)
      |> update_in(["MR"], &trunc(&1 * modifier))

    effect =
      if "Heal" in Map.keys(effect) do
        Ability.process_duration_trait(
          {"Heal", effect["Heal"]},
          effect,
          character,
          character,
          nil
        )
      else
        effect
      end

    character
    |> Systems.Effect.add(effect)
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

  def ability_for_weapon(character, weapon) do
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
        skill_level = Item.skill_for_character(character, weapon)

        modifier =
          if skill_level == 0 do
            0.1
          else
            skill_level / character.level
          end

        min_damage = weapon.min_damage * modifier
        max_damage = weapon.max_damage * modifier

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

    energy = Character.energy_per_swing(character, weapon)

    ability = %Ability{
      kind: "attack",
      energy: energy,
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
        "Dodgeable" => true,
        "DodgeUserMessage" =>
          "You #{singular_miss} {{target}} with your #{name}, but they dodge!",
        "DodgeTargetMessage" => "{{user}} #{plural_miss} you with their #{name}, but you dodge!",
        "DodgeSpectatorMessage" =>
          "{{user}} #{plural_miss} {{target}} with their #{name}, but they dodge!",
        "PhysicalPenetration" => Systems.Effect.effect_bonus(weapon, "PhysicalPenetration"),
        "MagicalPenetration" => Systems.Effect.effect_bonus(weapon, "MagicalPenetration")
      },
      limbs: limbs,
      skills: [weapon.weapon_type]
    }

    if on_hit = Systems.Effect.effect_bonus(weapon, "OnHit") do
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
          |> Map.put(:mana, 0)
          |> Map.put(:on_hit?, true)
          |> Map.put(:difficulty, nil)
        end)

      ability
      |> put_in([Access.key!(:traits), "OnHit"], on_hit)
      |> put_in([Access.key!(:traits), "OnHit%"], Systems.Effect.effect_bonus(weapon, "OnHit%"))

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
    player = Repo.get_by(Character, email: String.downcase(email))
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

  def used_experience(%Character{} = character) do
    character.classes
    |> Enum.reduce(0, fn character_class, used_experience ->
      level = character_class.level - 1
      class = Repo.get(Class, character_class.class_id)

      modifier = class.exp_modifier / 100

      exp = Level.exp_at_level(level, modifier)

      used_experience + exp
    end)
  end

  def trainable_experience(%Character{} = character) do
    character.experience - used_experience(character)
  end

  def add_attribute_experience(%Character{} = character, attribute, amount) do
    Character.pulse_score_attribute(character, attribute)

    attribute_level = character.attribute_levels[attribute]

    character =
      character
      # |> Ecto.Changeset.change(%{
      #   exp_buffer: Map.get(character, :exp_buffer) - amount
      # })
      # |> Repo.update!()
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

  def add_attribute_experience(%Character{} = character, %{} = attributes) do
    rate = drain_rate(character.level)

    hour_buffer = trunc(rate * 60 * 60)

    exp = max(1, hour_buffer * 0.01)

    Enum.reduce(attributes, character, fn {attribute, percent}, character ->
      amount = max(1, trunc(exp * percent))

      Character.add_attribute_experience(character, attribute, amount)
    end)
  end

  def add_attribute_experience(%{} = character, %{} = _attributes), do: character

  def add_skill_experience(%Character{} = character, skill_name, amount) do
    old_abilities = Map.values(character.abilities)
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

      character = Character.load_abilities(character)

      new_abilities = Map.values(character.abilities)

      Enum.each(new_abilities, fn ability ->
        unless ability in old_abilities do
          Mobile.send_scroll(
            character,
            "<p>\nYou've learned the <span class='dark-cyan'>#{ability.name}</span> ability!</p>"
          )

          Mobile.send_scroll(character, "<p>     #{ability.description}</p>")
        end
      end)

      character
    else
      character
    end
  end

  def exp_to_drain(%Character{exp_buffer: nil}), do: 0
  def exp_to_drain(%Character{exp_buffer: 0}), do: 0
  def exp_to_drain(%Character{exp_buffer_last_drained_at: nil}), do: 0

  def exp_to_drain(%Character{} = character) do
    max_level =
      case character.classes do
        [] ->
          1

        classes ->
          classes
          |> Enum.map(& &1.level)
          |> Enum.max()
      end

    exp_to_drain(character, max_level)
  end

  def exp_to_drain(%Character{exp_buffer_last_drained_at: time, exp_buffer: buffer}, level) do
    drain_total = Character.drain_rate(level)

    min(buffer, DateTime.diff(DateTime.utc_now(), time) * drain_total)
  end

  def drain_exp_buffer(%Character{} = character) do
    drain_total = exp_to_drain(character)

    character =
      if drain_total > 0 do
        mind = ApathyDrive.Commands.Status.mind(character)

        character =
          Enum.reduce(character.classes, character, fn class, character ->
            if class.experience == nil do
              character = update_in(character.classes, &List.delete(&1, class))

              exp =
                ApathyDrive.Commands.Train.required_experience(
                  character,
                  class.class_id,
                  class.level
                ) * 1.0

              class =
                class
                |> Ecto.Changeset.change(%{
                  experience: exp
                })
                |> Repo.update!()

              update_in(character.classes, &[class | &1])
            else
              character
            end
          end)

        # don't drain exp to classes that have enough to level
        drain_targets =
          Enum.reject(character.classes, fn class ->
            exp_to_level =
              ApathyDrive.Commands.Train.required_experience(
                character,
                class.class_id,
                class.level + 1
              )

            exp_to_level <= 0
          end)

        target_count = length(drain_targets)

        {character, drain_remaining} =
          drain_targets
          |> Enum.sort_by(& &1.level)
          |> Enum.with_index(1)
          |> Enum.reduce({character, drain_total}, fn {class, n}, {character, drain_remaining} ->
            if n == target_count do
              tnl = Level.exp_at_level(class.level, class.class.exp_modifier) - class.experience

              exp = min(drain_remaining, tnl)

              character = update_in(character.classes, &List.delete(&1, class))

              class =
                class
                |> Ecto.Changeset.change(%{
                  experience: class.experience + exp
                })
                |> Repo.update!()

              {update_in(character.classes, &[class | &1]), drain_remaining - exp}
            else
              rate = Character.drain_rate(class.level)

              tnl = Level.exp_at_level(class.level, class.class.exp_modifier) - class.experience

              exp = Enum.min([rate, drain_total / target_count, tnl])

              character = update_in(character.classes, &List.delete(&1, class))

              class =
                class
                |> Ecto.Changeset.change(%{
                  experience: class.experience + exp
                })
                |> Repo.update!()

              {update_in(character.classes, &[class | &1]), drain_remaining - exp}
            end
          end)

        amount = trunc(drain_total - drain_remaining)

        character =
          character
          |> Ecto.Changeset.change(%{
            exp_buffer: max(0, character.exp_buffer - amount)
          })
          |> Repo.update!()

        new_mind = ApathyDrive.Commands.Status.mind(character)

        if new_mind != mind do
          ApathyDrive.Commands.Status.status(character)
        end

        Character.update_exp_bar(character)
      else
        character
      end

    character
    |> Ecto.Changeset.change(%{
      exp_buffer_last_drained_at: DateTime.utc_now()
    })
    |> Repo.update!()
  end

  def add_experience_to_buffer(character, exp, silent \\ false)

  def add_experience_to_buffer(%Character{} = character, exp, silent) when exp > 0 do
    exp = trunc(exp)
    unless silent, do: Mobile.send_scroll(character, "<p>You gain #{exp} experience.</p>")

    mind = ApathyDrive.Commands.Status.mind(character)

    exp_buffer = character.exp_buffer + exp

    character =
      character
      |> Ecto.Changeset.change(%{
        exp_buffer: exp_buffer
      })
      |> Repo.update!()

    Statix.increment("exp_gained", exp, tags: ["character:#{String.downcase(character.name)}"])

    new_mind = ApathyDrive.Commands.Status.mind(character)

    if new_mind != mind do
      ApathyDrive.Commands.Status.status(character)
    end

    Character.update_exp_bar(character)
  end

  def add_experience_to_buffer(character, _exp, _silent), do: character

  def prompt(%Character{level: level, hp: hp_percent} = character) do
    max_mana = Mobile.max_mana_at_level(character, level)

    powerstone =
      Enum.reduce(character.inventory, 0, fn item, powerstone ->
        if "create powerstone" in item.enchantments do
          item.max_uses + powerstone
        else
          powerstone
        end
      end)

    max_mana = max_mana + powerstone

    hp = hp_at_level(character, level)
    mana = mana_at_level(character, level)

    resting =
      if character.resting do
        " (Resting) "
      end

    editing =
      if character.editing do
        " <span class='yellow'>*#{character.editing.name}*</span> "
      end

    lt =
      if character.enchantment do
        lt = Enum.find(TimerManager.timers(character), &match?({:longterm, _}, &1))
        tick_time_left = 67 - div(TimerManager.time_remaining(character, lt), 1000)
        time_left = Enchantment.time_left(character, character.enchantment)
        formatted = Enchantment.formatted_time_left(time_left - tick_time_left)
        "LT=#{formatted}"
      end

    mana =
      if max_mana > 0 do
        "MA=#{mana}"
      end

    hp = "HP=<span class='#{hp_prompt_color(hp_percent)}'>#{hp}</span>"

    prompt =
      [hp, mana, lt]
      |> Enum.reject(&is_nil/1)
      |> Enum.join("/")

    "[#{prompt}#{resting}#{editing}]:"
  end

  def hp_prompt_color(hp_percent) when hp_percent > 0.5, do: "grey"
  def hp_prompt_color(hp_percent) when hp_percent > 0.2, do: "dark-red"
  def hp_prompt_color(_hp_percent), do: "red"

  def hp_at_level(%Character{} = character, level) do
    max_hp = Mobile.max_hp_at_level(character, level)

    trunc(max_hp * character.hp)
  end

  def mana_at_level(%Character{} = character, level) do
    base_max_mana = Mobile.max_mana_at_level(character, level)

    powerstone_uses =
      Enum.reduce(character.inventory, 0, fn item, total ->
        if "create powerstone" in item.enchantments do
          item.uses + total
        else
          total
        end
      end)

    adjusted_max_mana = base_max_mana + powerstone_uses

    trunc(min(adjusted_max_mana, base_max_mana * character.mana + powerstone_uses))
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
         percentage: max(0, trunc(percent * 100)),
         round_length: Regeneration.round_length(mobile),
         max_percent: 100
       }}
    )

    character
  end

  def update_mana_bar(%Character{socket: socket} = character, mobile, _room) do
    mana = mana_at_level(character, character.level)

    powerstone =
      Enum.reduce(character.inventory, 0, fn item, powerstone ->
        if "create powerstone" in item.enchantments do
          item.max_uses + powerstone
        else
          powerstone
        end
      end)

    max_mana = Mobile.max_mana_at_level(character, character.level) + powerstone

    percent = mana / max_mana

    send(
      socket,
      {:update_mana_bar,
       %{
         ref: mobile.ref,
         player: mobile.ref == character.ref,
         percentage: max(0, trunc(percent * 100)),
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
         percentage: max(0, trunc(percent * 100)),
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

    max_hp = Mobile.max_hp_at_level(character, character.level)

    hp_regen = Mobile.hp_regen_per_30(character)

    damage_percent_per_30 =
      character
      |> Regeneration.damage_effect_per_tick()
      |> Regeneration.per_tick_to_per_30(character)

    damage_per_30 = damage_percent_per_30 * Mobile.max_hp_at_level(character, character.level)

    hp_regen = Float.round(hp_regen - damage_per_30, 2)

    powerstone =
      Enum.reduce(character.inventory, 0, fn item, powerstone ->
        if "create powerstone" in item.enchantments do
          item.max_uses + powerstone
        else
          powerstone
        end
      end)

    %{
      name: character.name,
      race: character.race.race.name,
      combat: character |> Character.combat_level() |> Character.combat_proficiency(),
      level: character.level,
      alignment: legal_status(character),
      perception: Mobile.perception_at_level(character, character.level, room),
      accuracy: Mobile.accuracy_at_level(character, character.level, room),
      crits: Mobile.crits_at_level(character, character.level),
      dodge: Mobile.dodge_at_level(character, character.level, room),
      stealth: Mobile.stealth_at_level(character, character.level),
      physical_resistance: Mobile.physical_resistance_at_level(character, character.level),
      magical_resistance: Mobile.magical_resistance_at_level(character, character.level),
      hp: hp_at_level(character, character.level),
      hp_regen: hp_regen,
      max_hp: max_hp,
      mana: mana_at_level(character, character.level),
      mana_regen: Float.round(Mobile.mana_regen_per_30(character), 2),
      max_mana: Mobile.max_mana_at_level(character, character.level) + powerstone,
      energy: character.energy,
      max_energy: character.max_energy,
      strength: Mobile.attribute_at_level(character, :strength, character.level),
      agility: Mobile.attribute_at_level(character, :agility, character.level),
      intellect: Mobile.attribute_at_level(character, :intellect, character.level),
      willpower: Mobile.attribute_at_level(character, :willpower, character.level),
      health: Mobile.attribute_at_level(character, :health, character.level),
      charm: Mobile.attribute_at_level(character, :charm, character.level),
      effects: effects,
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
    Mobile.ability_value(character, "CombatLevel")
  end

  def combat_proficiency(combat_level) when combat_level > 4.5, do: "Excellent"
  def combat_proficiency(combat_level) when combat_level > 3.5, do: "Good"
  def combat_proficiency(combat_level) when combat_level > 2.5, do: "Average"
  def combat_proficiency(combat_level) when combat_level > 1.5, do: "Fair"
  def combat_proficiency(_combat_level), do: "Poor"

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

  def drain_rate(level) do
    level = max(1, level)

    exp_to_level = Level.exp_at_level(level) - Level.exp_at_level(level - 1)

    target_time = 20 * level

    max(exp_to_level / (target_time * 60), 1.0)
  end

  def update_exp_bar(%Character{socket: socket} = character) do
    max_buffer = max(character.max_exp_buffer || 1, character.exp_buffer)

    percent = min(100, character.exp_buffer / max_buffer * 100)

    send(
      socket,
      {:update_exp_bar,
       %{
         percentage: percent
       }}
    )

    Map.put(character, :max_exp_buffer, max_buffer)
  end

  def update_attribute_bar(%Character{socket: socket} = character, attribute) do
    level = character.attribute_levels[attribute]

    exp = get_in(character, [Access.key!(:race), Access.key!(:"#{attribute}_experience")])
    modifier = (100 + character.race.race.exp_modifier) / 100

    current = Level.exp_at_level(level, modifier)
    to_level = Level.exp_at_level(level + 1, modifier)

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

  def evil_points_to_restore(%Character{evil_points_last_reduced_at: nil}), do: 0

  def evil_points_to_restore(%Character{evil_points_last_reduced_at: time}) do
    DateTime.diff(DateTime.utc_now(), time) * 0.01 / 60
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
          evil_points: min(300.0, character.evil_points + points)
        }
      else
        %{
          evil_points: max(-220.0, character.evil_points + points),
          evil_points_last_reduced_at: DateTime.utc_now()
        }
      end

    character =
      character
      |> Ecto.Changeset.cast(change, ~w(evil_points evil_points_last_reduced_at)a)
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
      Systems.Effect.effect_bonus(character, ability) || 0
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

    def attack_ability(character) do
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

      Character.ability_for_weapon(character, weapon)
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
          |> Map.put(:enchantment, nil)
          |> Ecto.Changeset.change(%{auto_roam: false})
          |> Repo.update!()
          |> Map.put(:attack_target, nil)
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
          |> TimerManager.send_after({:drain_exp, :timer.seconds(1), {:drain_exp, character.ref}})
          |> TimerManager.send_after(
            {:heartbeat, ApathyDrive.Regeneration.tick_time(character),
             {:heartbeat, character.ref}}
          )

        Room.start_room_id()
        |> RoomServer.find()
        |> RoomServer.mobile_entered(character)

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

    def hp_regen_per_30(%Character{hp: hp} = character) when hp >= 0 do
      if Enum.any?(character.limbs, fn {_name, limb} -> limb.health < 0 end) do
        0.0001
      else
        regen =
          (character.level + 20) * attribute_at_level(character, :health, character.level) / 750

        modified_hp_regen = regen * (1 + ability_value(character, "HPRegen") / 100)

        if character.resting do
          modified_hp_regen * 3
        else
          modified_hp_regen
        end
      end
    end

    def hp_regen_per_30(%Character{hp: _hp} = _character), do: 0.0

    def mana_regen_per_30(%Character{} = character) do
      level = character.level

      attribute =
        character.abilities
        |> Enum.map(fn {_command, ability} ->
          ability.attributes
          |> Enum.map(
            &Mobile.attribute_at_level(character, String.to_existing_atom(&1), character.level)
          )
        end)
        |> List.flatten()
        |> Room.average()

      magic_level = div(div(Mobile.max_mana_at_level(character, level) - 6, max(level, 1)), 2)

      max_mana = max_mana_at_level(character, character.level)

      base_mana_regen = (level + 20) * attribute * (magic_level + 2) / 1650.0

      rate = base_mana_regen * (1 + ability_value(character, "ManaRegen") / 100)

      if max_mana > 0 do
        if character.resting do
          rate * 3
        else
          rate
        end
      else
        0.0
      end
    end

    def heartbeat(%Character{} = character, %Room{} = room) do
      initial_hp = character.hp
      initial_max_hp = Mobile.max_hp_at_level(character, character.level)

      room =
        Room.update_mobile(room, character.ref, fn room, character ->
          room =
            room
            |> Ability.unbalance(character.ref)

          character = room.mobiles[character.ref]

          if character do
            character
            |> Regeneration.regenerate(room)
            |> TimerManager.send_after(
              {:heartbeat, ApathyDrive.Regeneration.tick_time(character),
               {:heartbeat, character.ref}}
            )
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

          Character.update_score(character, room)

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

      modifier = if max_hp_percent > 0, do: max_hp_percent, else: 1.0

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
      update_in(character.hp, &max(0.0, min(1.0, &1 + percentage)))
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

      mana_per_level = ability_value(character, "ManaPerLevel")

      magic_level = div(div(mana_per_level * (level - 1), max(character.level, 1)), 2)

      sc = trunc(attribute_value * 2 / 3) + magic_level * 5

      sc = sc + level * 2

      trunc(sc + ability_value(character, "Spellcasting"))
    end

    def stealth_at_level(character, level) do
      agility = attribute_at_level(character, :agility, level)
      charm = attribute_at_level(character, :charm, level)

      base = div(agility * 3 + charm, 6) + level * 2

      race_modifier =
        if character.race.race.stealth do
          0.4
        else
          0
        end

      skill = character.skills["stealth"]

      class_modifier =
        if level > 0 && skill && skill.level > 0 do
          skill.level / level * 0.6
        else
          0
        end

      modifier = race_modifier + class_modifier

      modified = trunc(base * modifier)

      ability = ability_value(character, "Stealth")

      max(0, modified + ability)
    end

    def subtract_mana(character, %{mana: 0} = _ability), do: character

    def subtract_mana(character, %Ability{mana: cost}) do
      {cost, character} =
        Enum.reduce(character.inventory, {cost, character}, fn item, {cost, character} ->
          if "create powerstone" in item.enchantments do
            amount = min(cost, item.uses)
            cost = cost - amount
            character = update_in(character.inventory, &List.delete(&1, item))
            item = update_in(item.uses, &(&1 - amount))
            character = update_in(character.inventory, &[item | &1])
            {cost, character}
          else
            {cost, character}
          end
        end)

      max_mana = Mobile.max_mana_at_level(character, character.level)

      if max_mana > 0 do
        percentage = cost / max_mana

        character
        |> update_in([Access.key!(:mana)], &max(0, &1 - percentage))
      else
        character
        |> put_in([Access.key!(:mana)], 0.0)
      end
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
      send(socket, {:update_prompt, Character.prompt(character)})
      character
    end
  end
end
