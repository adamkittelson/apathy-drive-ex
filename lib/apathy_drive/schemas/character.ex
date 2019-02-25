defmodule ApathyDrive.Character do
  use Ecto.Schema
  use ApathyDriveWeb, :model

  alias ApathyDrive.{
    Ability,
    AbilityAttribute,
    AbilityDamageType,
    AbilityTrait,
    AI,
    Character,
    CharacterSkill,
    Class,
    ClassTrait,
    Companion,
    Currency,
    Directory,
    Regeneration,
    Item,
    Level,
    Monster,
    Mobile,
    Party,
    Race,
    RaceTrait,
    Room,
    RoomServer,
    Skill,
    Statix,
    Text,
    TimerManager,
    Title
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
    field(:max_exp_buffer, :integer, default: 0)
    field(:strength_experience, :integer, default: 0)
    field(:agility_experience, :integer, default: 0)
    field(:intellect_experience, :integer, default: 0)
    field(:willpower_experience, :integer, default: 0)
    field(:health_experience, :integer, default: 0)
    field(:charm_experience, :integer, default: 0)
    field(:timers, :map, virtual: true, default: %{})
    field(:admin, :boolean)
    field(:flags, :map, default: %{})
    field(:copper, :integer, default: 0)
    field(:silver, :integer, default: 0)
    field(:gold, :integer, default: 0)
    field(:platinum, :integer, default: 0)
    field(:runic, :integer, default: 0)
    field(:race_id, :integer)
    field(:class_id, :integer)
    field(:pity_modifier, :integer, default: 0)
    field(:auto_heal, :boolean)
    field(:auto_bless, :boolean)
    field(:auto_curse, :boolean)
    field(:auto_nuke, :boolean)
    field(:auto_roam, :boolean)
    field(:auto_flee, :boolean)
    field(:bounty, :integer, default: 0)
    field(:alignment, :string)

    field(:level, :integer, virtual: true, default: 1)
    field(:race, :string, virtual: true)
    field(:class, :string, virtual: true)
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
    field(:combat_level, :integer, virtual: true, default: 3)
    field(:energy, :integer, virtual: true, default: 1000)
    field(:max_energy, :integer, virtual: true, default: 1000)
    field(:reply_to, :string, virtual: true)
    field(:casting, :any, virtual: true, default: nil)
    field(:weapon, :string, virtual: true)
    field(:armour, :string, virtual: true)
    field(:mana_regen_attributes, :any, virtual: true, default: [])
    field(:last_tick_at, :any, virtual: true)
    field(:last_room_id, :integer, virtual: true)
    field(:delayed, :boolean, virtual: true, default: false)
    field(:sneaking, :boolean, virtual: true, default: false)

    belongs_to(:room, Room)

    has_many(:items_instances, ApathyDrive.ItemInstance)

    has_many(:characters_skills, ApathyDrive.CharacterSkill)
    has_many(:trained_skills, through: [:characters_skills, :skill])

    timestamps()
  end

  def legal_status(%Character{bounty: bounty}) when bounty >= 1_000_000, do: "FIEND"
  def legal_status(%Character{bounty: bounty}) when bounty >= 500_000, do: "Villain"
  def legal_status(%Character{bounty: bounty}) when bounty >= 250_000, do: "Criminal"
  def legal_status(%Character{bounty: bounty}) when bounty >= 10_000, do: "Outlaw"
  def legal_status(%Character{bounty: bounty}) when bounty > 0, do: "Seedy"
  def legal_status(%Character{bounty: _bounty}), do: "Lawful"

  def wearing_enchanted_item?(%Character{} = character, %Ability{} = ability) do
    Enum.any?(character.equipment, fn item ->
      Item.enchantment(item) == ability.id
    end)
  end

  def wearing_enchanted_item?(%{} = _mobile, %Ability{}), do: false

  def set_title(%Character{} = character) do
    Map.put(character, :title, Title.for_character(character))
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
      |> load_items()
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
      character.strength * 48 * (1 + Systems.Effect.effect_bonus(character, "Encumbrance") / 100)
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
    |> cast(params, ~w(email password name race_id gender class_id alignment))
    |> validate_required(~w(email password name race_id gender class_id alignment)a)
    |> validate_format(:email, ~r/@/)
    |> validate_length(:email, min: 3, max: 100)
    |> validate_length(:password, min: 6)
    |> unique_constraint(:email, name: :characters_lower_email_index, on: Repo)
    |> validate_confirmation(:password)
    |> validate_inclusion(:race_id, ApathyDrive.Race.ids())
    |> validate_inclusion(:class_id, ApathyDrive.Class.ids())
    |> validate_inclusion(:gender, ["male", "female"])
    |> validate_inclusion(:alignment, ["good", "neutral", "evil"])
    |> validate_format(:name, ~r/^[a-zA-Z]+$/)
    |> unique_constraint(:name, name: :characters_lower_name_index, on: Repo)
    |> validate_length(:name, min: 1, max: 12)
  end

  def load_abilities(%Character{} = character) do
    character =
      character
      |> Map.put(:abilities, %{})
      |> Map.put(:skills, %{})

    abilities =
      character.class_id
      |> ApathyDrive.ClassAbility.abilities_at_level(character.level)

    character =
      abilities
      |> Enum.reduce(character, fn
        %{ability: %Ability{id: id, kind: "passive"}}, character ->
          effect = AbilityTrait.load_traits(id)
          Systems.Effect.add(character, effect)

        %{ability: %Ability{id: id} = ability}, character ->
          ability =
            ability
            |> put_in([Access.key!(:traits)], AbilityTrait.load_traits(id))

          attributes = AbilityAttribute.load_attributes(id)

          ability =
            if map_size(attributes) == 0 do
              Map.put(ability, :attributes, %{willpower: 40})
            else
              Map.put(ability, :attributes, attributes)
            end

          ability =
            case AbilityDamageType.load_damage(id) do
              [] ->
                ability

              damage ->
                update_in(ability.traits, &Map.put(&1, "Damage", damage))
            end

          update_in(character.abilities, fn abilities ->
            Map.put(abilities, ability.command, ability)
          end)
      end)

    Enum.reduce(character.equipment, character, fn item, character ->
      if ability = item.traits["Grant"] do
        put_in(character.abilities[ability.command], ability)
      else
        character
      end
    end)
  end

  def set_attribute_levels(%Character{} = character) do
    character =
      [:strength, :agility, :intellect, :willpower, :health, :charm]
      |> Enum.reduce(character, fn stat, character ->
        exp = Map.get(character, :"#{stat}_experience")

        level = Level.level_at_exp(exp, 1.0)

        character =
          character
          |> put_in([Access.key!(:attribute_levels), stat], level)
          |> put_in([Access.key!(stat)], character.race[stat] + level)

        Character.update_attribute_bar(character, stat)
        character
      end)

    Map.put(character, :level, level(character))
  end

  def level(%Character{} = character) do
    average_attribute_level =
      character.attribute_levels
      |> Map.values()
      |> Enum.sum()
      |> div(6)

    average_attribute_level + 1
  end

  def load_race(%Character{race_id: race_id} = character) do
    race = Repo.get(Race, race_id)

    effect =
      RaceTrait.load_traits(race_id)
      |> Map.put("stack_key", "race")
      |> Map.put("stack_count", 1)

    race =
      Map.take(race, [
        :name,
        :strength,
        :agility,
        :intellect,
        :willpower,
        :health,
        :charm,
        :stealth
      ])

    character
    |> Map.put(:race, race)
    |> Systems.Effect.add(effect)
  end

  def load_class(%Character{class_id: class_id} = character) do
    class = Repo.get(Class, class_id)

    effect =
      ClassTrait.load_traits(class_id)
      |> Map.put("stack_key", "class")
      |> Map.put("stack_count", 1)

    character
    |> Map.put(:class, Map.take(class, [:name, :stealth]))
    |> Map.put(:weapon, class.weapon)
    |> Map.put(:armour, class.armour)
    |> Systems.Effect.add(effect)
  end

  def load_items(%Character{} = character) do
    items = ApathyDrive.ItemInstance.load_items(character)

    character
    |> Map.put(:inventory, Enum.reject(items, & &1.equipped))
    |> Map.put(:equipment, Enum.filter(items, & &1.equipped))
    |> add_equipped_items_effects()
    |> load_abilities()
    |> TimerManager.send_after(
      {:use_light_source, :timer.seconds(30), {:use_light_source, character.ref}}
    )
  end

  def add_equipped_items_effects(%Character{} = character) do
    character =
      Enum.reduce(character.effects, character, fn {_key, %{"stack_key" => key}}, character ->
        if is_binary(key) and String.starts_with?(key, "item") do
          Systems.Effect.remove_oldest_stack(character, key)
        else
          character
        end
      end)

    Enum.reduce(character.equipment, character, fn item, updated_character ->
      add_equipped_item_effects(updated_character, item)
    end)
  end

  def add_equipped_item_effects(%Character{} = character, item) do
    {ability_id, traits} =
      case item.traits do
        %{"Passive" => ability} ->
          traits =
            Enum.reduce(ability.traits, item.traits, fn {trait, value}, traits ->
              if trait in Map.keys(traits) do
                cond do
                  is_integer(traits[trait]) ->
                    Map.put(traits, trait, traits[trait] + value)

                  is_list(traits[trait]) ->
                    Map.put(traits, trait, [value | traits[trait]])

                  :else ->
                    Map.put(traits, trait, value)
                end
              else
                Map.put(traits, trait, value)
              end
            end)

          {ability.id, traits}

        traits ->
          {nil, traits}
      end

    character =
      if ability_id do
        effect = Map.put(traits, "stack_key", "item-#{item.instance_id}")

        character
        |> Systems.Effect.remove_oldest_stack(ability_id)
        |> Systems.Effect.add(effect)
      else
        effect = Map.put(traits, "stack_key", "item-#{item.instance_id}")

        character
        |> Systems.Effect.add(effect)
      end

    load_abilities(character)
  end

  def sanitize(message) do
    {:safe, message} = Phoenix.HTML.html_escape(message)

    message
  end

  def weapon(%Character{} = character) do
    character.equipment
    |> Enum.find(&(&1.worn_on in ["Weapon Hand", "Two Handed"]))
  end

  def ability_for_weapon(character, weapon) do
    %Item{
      type: "Weapon",
      name: name,
      hit_verbs: hit_verbs,
      miss_verbs: [singular_miss, plural_miss],
      min_damage: min_damage,
      max_damage: max_damage
    } = weapon

    [singular_hit, plural_hit] = Enum.random(hit_verbs)

    energy = Character.energy_per_swing(character, weapon)

    ability = %Ability{
      kind: "attack",
      energy: energy,
      name: weapon.name,
      attributes: %{
        strength: 0,
        agility: 0
      },
      mana: 0,
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
            damage_type: "Normal",
            min: min_damage,
            max: max_damage
          }
        ],
        "Dodgeable" => true,
        "DodgeUserMessage" =>
          "You #{singular_miss} {{target}} with your #{name}, but they dodge!",
        "DodgeTargetMessage" => "{{user}} #{plural_miss} you with their #{name}, but you dodge!",
        "DodgeSpectatorMessage" =>
          "{{user}} #{plural_miss} {{target}} with their #{name}, but they dodge!"
      }
    }

    if on_hit = weapon.traits["OnHit"] do
      if on_hit.kind == "attack" do
        modifier = energy / 1000

        Enum.reduce(on_hit.traits["Damage"], ability, fn damage, ability ->
          damage =
            damage
            |> update_in([:min], &trunc(&1 * modifier))
            |> update_in([:max], &trunc(&1 * modifier))

          update_in(ability, [Access.key!(:traits), "Damage"], &[damage | &1])
        end)
      else
        on_hit =
          on_hit
          |> Map.put(:energy, 0)
          |> Map.put(:mana, 0)
          |> Map.put(:on_hit?, true)

        put_in(ability.traits["OnHit"], on_hit)
      end
    else
      ability
    end
  end

  def shield(%Character{} = character) do
    character.equipment
    |> Enum.find(&(&1.worn_on == "Off-Hand" and &1.type == "Shield"))
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

  def add_attribute_experience(%Character{exp_buffer: buffer} = character, attribute, amount)
      when buffer >= amount do
    Character.pulse_score_attribute(character, attribute)

    character_level = character.level

    attribute_level = character.attribute_levels[attribute]

    character =
      character
      |> Ecto.Changeset.change(%{
        "#{attribute}_experience": Map.get(character, :"#{attribute}_experience") + trunc(amount),
        exp_buffer: Map.get(character, :exp_buffer) - amount
      })
      |> Repo.update!()

    new_attribute_level =
      character
      |> Map.get(:"#{attribute}_experience")
      |> Level.level_at_exp(1.0)

    character =
      if new_attribute_level > attribute_level do
        old_abilities = Map.values(character.abilities)

        character =
          character
          |> Character.set_attribute_levels()
          |> Character.load_abilities()
          |> Character.set_title()

        new_abilities = Map.values(character.abilities)

        Mobile.send_scroll(
          character,
          "<p><span class='yellow'>Your #{attribute} increases to #{Map.get(character, attribute)}!</span></p>"
        )

        if character.level > character_level do
          Mobile.send_scroll(
            character,
            "<p><span class='yellow'>Your level increases to #{character.level}!</span></p>"
          )

          Directory.add_character(%{
            name: character.name,
            bounty: character.bounty,
            room: character.room_id,
            ref: character.ref,
            title: character.title
          })
        end

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

    Character.update_attribute_bar(character, attribute)
    Character.update_exp_bar(character)

    character
  end

  def add_attribute_experience(%Character{} = character, _attribute, _amount), do: character

  def add_attribute_experience(%Character{} = character, %{} = attributes) do
    exp = max(1, trunc(character.exp_buffer * 0.01))

    Enum.reduce(attributes, character, fn {attribute, percent}, character ->
      amount = max(1, trunc(exp * percent))

      Character.add_attribute_experience(character, attribute, amount)
    end)
  end

  def add_attribute_experience(%{} = character, %{} = _attributes), do: character

  def drain_exp_buffer(%Character{exp_buffer: 0} = character), do: character

  def drain_exp_buffer(%Character{} = character) do
    sorted_attributes =
      Enum.sort_by([:strength, :agility, :intellect, :willpower, :health, :charm], fn attribute ->
        Map.get(character, attribute)
      end)

    attribute =
      sorted_attributes
      |> Enum.filter(
        &(Map.get(character, &1) == Map.get(character, List.first(sorted_attributes)))
      )
      |> Enum.random()

    Character.add_attribute_experience(character, attribute, 1)
  end

  def add_experience(%Character{} = character, exp) when exp > 0 do
    Mobile.send_scroll(character, "<p>You gain #{exp} experience.</p>")

    exp_buffer = character.exp_buffer + exp

    character =
      character
      |> Ecto.Changeset.change(%{
        exp_buffer: exp_buffer,
        max_exp_buffer: max(exp_buffer, character.max_exp_buffer)
      })
      |> Repo.update!()

    Statix.increment("exp_gained", exp, tags: ["character:#{String.downcase(character.name)}"])

    Character.update_exp_bar(character)
  end

  def add_experience(character, _exp), do: character

  def train_skill(%Character{} = character, %Skill{} = skill, amount) when amount > 0 do
    skill = Repo.preload(skill, :incompatible_skills)
    incompatible_skills = Enum.map(skill.incompatible_skills, & &1.name)

    Repo.insert(
      %CharacterSkill{
        character_id: character.id,
        skill_id: skill.id,
        experience: skill.experience + amount
      },
      on_conflict: :replace_all,
      conflict_target: [:character_id, :skill_id]
    )

    old_abilities = Map.values(character.abilities)

    character = load_abilities(character)

    new_abilities = Map.values(character.abilities)

    level = character.skills[skill.name].level

    tnl =
      Level.exp_to_next_skill_level(
        level,
        character.skills[skill.name].experience,
        skill.training_cost_multiplier
      )

    tnl =
      if tnl > character.experience do
        "<span class='dark-red'>#{tnl}</span>"
      else
        "<span class='green'>#{tnl}</span>"
      end

    Mobile.send_scroll(
      character,
      "<p>You spend #{amount} experience to advance #{skill.name} to level #{level}, it will cost #{
        tnl
      } experience to train it any further.</p>"
    )

    Enum.each(new_abilities, fn ability ->
      unless ability in old_abilities do
        Mobile.send_scroll(
          character,
          "<p>\nYou've learned the <span class='dark-cyan'>#{ability.name}</span> ability!</p>"
        )

        Mobile.send_scroll(character, "<p>     #{ability.description}</p>")
      end
    end)

    character.skills
    |> Enum.reduce(character, fn {skill_name, %Skill{experience: skill_exp}}, character ->
      if skill_name in incompatible_skills do
        incompatible_skill = Skill.match_by_name(skill_name)
        level = Level.level_at_exp(skill_exp, incompatible_skill.training_cost_multiplier)

        if level > 0 do
          new_level = level - 1
          new_exp = Level.exp_at_level(level, incompatible_skill.training_cost_multiplier)

          Repo.insert(
            %CharacterSkill{
              character_id: character.id,
              skill_id: incompatible_skill.id,
              experience: new_exp
            },
            on_conflict: :replace_all,
            conflict_target: [:character_id, :skill_id]
          )

          Mobile.send_scroll(
            character,
            "<p>Your #{incompatible_skill.name} skill falls to level #{new_level}.</p>"
          )

          old_abilities = Map.values(character.abilities)

          character = load_abilities(character)

          new_abilities = Map.values(character.abilities)

          Enum.each(old_abilities, fn ability ->
            unless ability in new_abilities do
              Mobile.send_scroll(
                character,
                "<p>You've forgotten the <span class='dark-cyan'>#{ability.name}</span> ability.</p>"
              )
            end
          end)

          character
        else
          character
        end
      else
        character
      end
    end)
  end

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
      "[HP=<span class='#{hp_prompt_color(hp_percent)}'>#{hp}</span>/MA=#{mana}]:"
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

  def update_energy_bar(%Character{socket: socket} = character, mobile) do
    percent = mobile.energy / mobile.max_energy

    send(
      socket,
      {:update_energy_bar,
       %{
         ref: mobile.ref,
         player: mobile.ref == character.ref,
         percentage: trunc(percent * 100),
         time_to_full: Mobile.round_length_in_ms(mobile) * (1 - percent)
       }}
    )

    character
  end

  def update_mana_bar(%Character{socket: socket} = character, mobile, room) do
    percent = mobile.mana

    time_to_full =
      if percent == 1.0 do
        0
      else
        regen_per_tick =
          Regeneration.regen_per_tick(room, mobile, Mobile.mana_regen_per_round(mobile))

        ticks_remaining = (1.0 - percent) / regen_per_tick

        Regeneration.tick_time(mobile) * Float.ceil(ticks_remaining)
      end

    send(
      socket,
      {:update_mana_bar,
       %{
         ref: mobile.ref,
         player: mobile.ref == character.ref,
         percentage: trunc(percent * 100),
         time_to_full: time_to_full
       }}
    )

    character
  end

  def update_mana_bar(%{} = character), do: character

  def update_hp_bar(%Character{socket: socket} = character, mobile, room) do
    percent = mobile.hp

    time_to_full =
      if percent == 1.0 do
        0
      else
        regen_per_tick =
          Regeneration.regen_per_tick(room, mobile, Mobile.hp_regen_per_round(mobile))

        ticks_remaining = (1.0 - percent) / regen_per_tick

        Regeneration.tick_time(mobile) * Float.ceil(ticks_remaining)
      end

    send(
      socket,
      {:update_hp_bar,
       %{
         ref: mobile.ref,
         player: mobile.ref == character.ref,
         percentage: trunc(percent * 100),
         time_to_full: time_to_full
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

    resistances =
      ApathyDrive.DamageType
      |> Repo.all()
      |> Enum.reduce(%{}, fn damage_type, resistances ->
        Map.put(
          resistances,
          damage_type.name,
          Mobile.ability_value(character, "Resist#{damage_type.name}")
        )
      end)

    %{
      name: character.name,
      race: character.race.name,
      class: character.class.name,
      level: character.level,
      alignment: character.alignment,
      perception: Mobile.perception_at_level(character, character.level, room),
      accuracy: Mobile.accuracy_at_level(character, character.level, room),
      crits: Mobile.crits_at_level(character, character.level),
      dodge: Mobile.dodge_at_level(character, character.level, room),
      stealth: Mobile.stealth_at_level(character, character.level),
      block: Mobile.block_at_level(character, character.level),
      parry: Mobile.parry_at_level(character, character.level),
      physical_resistance: Mobile.physical_resistance_at_level(character, character.level),
      magical_damage: Mobile.magical_damage_at_level(character, character.level),
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
      resistances: resistances,
      round_length_in_ms: Mobile.round_length_in_ms(character)
    }
  end

  def energy_per_swing(character, weapon \\ nil) do
    weapon = weapon || Character.weapon(character)
    encumbrance = Character.encumbrance(character)
    max_encumbrance = Character.max_encumbrance(character)

    cost =
      weapon.speed * 1000 /
        ((character.level * (character.combat_level + 2) + 45) * (character.agility + 150) * 1500 /
           9000.0)

    cost =
      if character.strength < weapon.required_strength do
        ((weapon.required_strength - character.strength) * 3 + 200) * cost / 200.0
      else
        cost
      end

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

  def update_exp_bar(%Character{socket: socket} = character) do
    percent =
      if character.max_exp_buffer > 0 do
        trunc(character.exp_buffer / character.max_exp_buffer * 100)
      else
        0
      end

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

    exp = Map.get(character, :"#{attribute}_experience")
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

  defimpl ApathyDrive.Mobile, for: Character do
    def ability_value(character, ability) do
      character_value = Systems.Effect.effect_bonus(character, ability)

      equipment_value =
        Enum.reduce(character.equipment, 0, fn item, total ->
          total + Systems.Effect.effect_bonus(item, ability)
        end)

      character_value + equipment_value
    end

    def accuracy_at_level(character, _level, _room) do
      agi = character.agility + character.charm / 10
      modifier = ability_value(character, "Accuracy")
      trunc(agi * (1 + modifier / 100))
    end

    def attribute_at_level(%Character{} = character, attribute, _level) do
      Map.get(character, attribute)
    end

    def attack_ability(character) do
      punch = %Item{
        type: "Weapon",
        name: "fist",
        hit_verbs: [["punch", "punches"]],
        miss_verbs: ["throw a punch", "throws a punch"],
        min_damage: 2,
        max_damage: 7,
        speed: 1150,
        required_strength: 0
      }

      weapon = Character.weapon(character) || punch

      Character.ability_for_weapon(character, weapon)
    end

    def auto_attack_target(%Character{attack_target: target} = _character, room) do
      if room.mobiles[target], do: target
    end

    def caster_level(%Character{level: caster_level}, %{} = _target), do: caster_level

    def cpr(%Character{} = character) do
      time =
        min(
          Mobile.round_length_in_ms(character),
          TimerManager.time_remaining(character, :heartbeat)
        )

      character
      |> TimerManager.send_after({:heartbeat, time, {:heartbeat, character.ref}})
      |> TimerManager.send_after(
        {:reduce_bounty, :timer.seconds(60), {:reduce_bounty, character.ref}}
      )
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

    def color(%Character{alignment: "evil"}), do: "magenta"
    def color(%Character{alignment: "neutral"}), do: "dark-cyan"
    def color(%Character{alignment: "good"}), do: "grey"

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
      character_level = Mobile.target_level(observer, character)
      observer_level = Mobile.caster_level(observer, character)

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
        character.race.name
      }. {{target:He moves/She moves/They move}} #{descriptions[:agility]}, and {{target:is/is/are}} #{
        descriptions[:charm]
      } #{character.name} appears to be #{descriptions[:intellect]} and #{
        descriptions[:willpower]
      }."
    end

    def die(character, room) do
      character =
        character
        |> Mobile.send_scroll("<p><span class='red'>You have died.</span></p>")
        |> Map.put(:hp, 1.0)
        |> Map.put(:mana, 1.0)
        |> Map.put(:energy, character.max_energy)
        |> Map.put(:attack_target, nil)
        |> Map.put(:attack_roam, false)
        |> update_in([:effects], fn effects ->
          effects
          |> Enum.filter(fn {_key, effect} -> effect["stack_key"] in ["race", "class"] end)
          |> Enum.into(%{})
        end)
        |> Map.put(:timers, %{})
        |> Character.add_equipped_items_effects()
        |> Mobile.update_prompt()
        |> Mobile.cpr()

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

    def dodge_at_level(character, level, room) do
      skill = character.skills["dodge"] || Repo.get_by(Skill, name: "dodge")
      skill_level = skill.level
      level = min(level, skill_level)

      agi = attribute_at_level(character, :agility, level)
      cha = Party.charm_at_level(room, character, level)
      agi = agi + cha / 10
      modifier = ability_value(character, "Dodge")
      trunc(agi * (1 + modifier / 100))
    end

    def block_at_level(character, level) do
      skill = character.skills["shield"] || Repo.get_by(Skill, name: "shield")
      skill_level = skill.level
      level = min(level, skill_level)

      str = Mobile.attribute_at_level(character, :strength, level)
      cha = Mobile.attribute_at_level(character, :charm, level)
      str = str + cha / 10
      modifier = Mobile.ability_value(character, "Block")
      trunc(str * (1 + modifier / 100))
    end

    def parry_at_level(character, _level) do
      str = Mobile.attribute_at_level(character, :strength, character.level)
      agi = Mobile.attribute_at_level(character, :agility, character.level)
      cha = Mobile.attribute_at_level(character, :charm, character.level)
      raw = (str + agi) / 2 + cha / 10
      modifier = Mobile.ability_value(character, "Parry")
      trunc(raw * (1 + modifier / 100))
    end

    def enough_mana_for_ability?(character, %Ability{mana: cost} = _ability) do
      mana = Character.mana_at_level(character, character.level)

      mana >= cost
    end

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

    def hp_regen_per_round(%Character{} = character) do
      round_length = Mobile.round_length_in_ms(character)

      base_hp_regen =
        (character.level + 30) * attribute_at_level(character, :health, character.level) / 500.0 *
          round_length / 30_000

      modified_hp_regen = base_hp_regen * (1 + ability_value(character, "HPRegen") / 100)

      max_hp = max_hp_at_level(character, character.level)

      modified_hp_regen / max_hp
    end

    def mana_regen_per_round(%Character{} = character) do
      round_length = Mobile.round_length_in_ms(character)

      max_mana = max_mana_at_level(character, character.level)

      attribute_value =
        character.mana_regen_attributes
        |> Enum.map(&attribute_at_level(character, &1, character.level))
        |> Enum.sum()
        |> div(length(character.mana_regen_attributes))

      base_mana_regen =
        (character.level + 20) * attribute_value *
          (div(ability_value(character, "ManaPerLevel"), 2) + 2) / 1650.0 * round_length / 30_000

      modified_mana_regen = base_mana_regen * (1 + ability_value(character, "ManaRegen") / 100)

      if max_mana > 0 do
        modified_mana_regen / max_mana
      else
        0
      end
    end

    def heartbeat(%Character{} = character, %Room{} = room) do
      Room.update_mobile(room, character.ref, fn character ->
        character
        |> Regeneration.regenerate(room)
        |> Character.drain_exp_buffer()
        |> RoomServer.execute_casting_ability(room)
      end)
      |> AI.think(character.ref)
    end

    def exhausted(%{energy: energy} = character) do
      required_energy = ApathyDrive.Commands.Move.energy_cost(character)

      if energy < required_energy do
        send_scroll(character, "<p>You're too tired to move!</p>")
        true
      else
        false
      end
    end

    def held(%{effects: effects} = mobile) do
      effects
      |> Map.values()
      |> Enum.find(fn effect ->
        Map.has_key?(effect, "held")
      end)
      |> held(mobile)
    end

    def held(nil, %{}), do: false

    def held(%{"effect_message" => message}, %{} = mobile) do
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

    def magical_damage_at_level(character, level) do
      attribute = attribute_at_level(character, :intellect, level) - 50

      attribute + ability_value(character, "ModifyDamage") +
        ability_value(character, "ModifyMagicalDamage")
    end

    def magical_resistance_at_level(character, level) do
      willpower = attribute_at_level(character, :willpower, level)

      max(willpower - 50 + ability_value(character, "MagicalResist"), 0)
    end

    def max_hp_at_level(mobile, level) do
      health = attribute_at_level(mobile, :health, level)

      base = health / 2
      hp_per_level = ability_value(mobile, "HPPerLevel") * level
      bonus = (health - 50) * level / 16

      modifier = ability_value(mobile, "MaxHP")
      trunc((base + hp_per_level + bonus) * (1 + modifier / 100))
    end

    def max_mana_at_level(mobile, level) do
      mana_per_level = ability_value(mobile, "ManaPerLevel")

      mana_per_level * (level - 1) + 6
    end

    def party_refs(character, room) do
      Party.refs(room, character)
    end

    def perception_at_level(character, level, room) do
      int = attribute_at_level(character, :intellect, level)
      cha = Party.charm_at_level(room, character, level)
      int = int + cha / 10
      modifier = ability_value(character, "Perception")
      trunc(int * (1 + modifier / 100))
    end

    def physical_damage_at_level(character, level) do
      attribute = attribute_at_level(character, :strength, level) - 50

      attribute + ability_value(character, "ModifyDamage") +
        ability_value(character, "ModifyPhysicalDamage")
    end

    def physical_resistance_at_level(character, level) do
      strength = attribute_at_level(character, :strength, level)
      ac = ability_value(character, "AC")

      max(strength - 50 + ac, 0)
    end

    def power_at_level(%Character{} = character, level) do
      [:strength, :agility, :intellect, :willpower, :health, :charm]
      |> Enum.reduce(0, &(&2 + Mobile.attribute_at_level(character, &1, level)))
    end

    def round_length_in_ms(character) do
      speed = ability_value(character, "Speed")

      modifier =
        if speed == 0 do
          1
        else
          speed / 100
        end

      trunc(modifier * Application.get_env(:apathy_drive, :round_length_in_ms))
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
        bounty: character.bounty,
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

    def shift_hp(character, percentage, room \\ nil) do
      hp_description = hp_description(character)
      character = update_in(character.hp, &min(1.0, &1 + percentage))
      updated_hp_description = hp_description(character)

      if room && (character.hp > 0 and hp_description != updated_hp_description) do
        room.mobiles
        |> Map.values()
        |> Enum.reject(&(&1.ref == character.ref))
        |> Enum.each(fn
          %Character{} = observer ->
            Mobile.send_scroll(
              observer,
              "<p>#{Mobile.colored_name(character)} is #{updated_hp_description}.</p>"
            )

          _ ->
            :noop
        end)
      end

      character
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
      sc =
        ability.attributes
        |> Map.keys()
        |> Enum.map(&Mobile.attribute_at_level(character, &1, level))
        |> Enum.sum()
        |> div(map_size(ability.attributes))

      sc + ability_value(character, "Spellcasting")
    end

    def abilities_at_level(%Character{abilities: abilities}, level) do
      abilities
      |> Map.values()
      |> Enum.filter(&(&1.level <= level))
      |> Enum.sort_by(& &1.level)
    end

    def stealth_at_level(character, level) do
      agility = attribute_at_level(character, :agility, level)
      charm = attribute_at_level(character, :charm, level)

      base = div(agility * 3 + charm, 6) + level * 2

      modifier =
        cond do
          !character.race.stealth and !character.class.stealth ->
            0

          !character.class.stealth ->
            0.4

          !character.race.stealth ->
            0.6

          :else ->
            1
        end

      max(0, trunc(base * modifier) + ability_value(character, "Stealth"))
    end

    def subtract_mana(character, %{mana: cost} = ability) do
      percentage = cost / Mobile.max_mana_at_level(character, character.level)

      character
      |> update_in([Access.key!(:mana)], &max(0, &1 - percentage))
      |> update_in(
        [Access.key!(:mana_regen_attributes)],
        &Enum.uniq(&1 ++ Map.keys(ability.attributes))
      )
    end

    def subtract_energy(character, ability) do
      character = update_in(character.energy, &max(0, &1 - ability.energy))

      Enum.reduce(ability.attributes, character, fn {attribute, _value}, character ->
        Character.add_attribute_experience(character, %{
          attribute => 1 / length(Map.keys(ability.attributes))
        })
      end)
    end

    def target_level(%Character{level: _caster_level}, %Character{level: target_level}),
      do: target_level

    def target_level(%Character{level: _caster_level}, %Companion{level: target_level}),
      do: target_level

    def target_level(%Character{level: caster_level}, %Monster{level: target_level}),
      do: max(caster_level, target_level)

    def tracking_at_level(character, level, room) do
      perception = perception_at_level(character, level, room)
      modifier = ability_value(character, "Tracking")
      perception * (modifier / 100)
    end

    def update_prompt(%Character{socket: socket} = character) do
      send(socket, {:update_prompt, Character.prompt(character)})
      character
    end
  end
end
