defmodule ApathyDrive.Character do
  use Ecto.Schema
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Ability, Character, EntityAbility, EntityItem, Item, ItemAbility, Mobile, Race, Room, RoomServer, Spell, SpellAbility, Text, TimerManager}

  require Logger
  import Comeonin.Bcrypt

  schema "characters" do
    field :name,            :string
    field :gender,          :string
    field :email,           :string
    field :password,        :string
    field :external_id,     :string
    field :experience,      :integer, default: 0
    field :level,           :integer, default: 1
    field :timers,          :map, virtual: true, default: %{}
    field :admin,           :boolean
    field :flags,           :map, default: %{}
    field :gold,            :integer, default: 150
    field :race_id,         :integer
    field :pity_modifier,   :integer, default: 0
    field :race,            :string, virtual: true
    field :monitor_ref,     :any, virtual: true
    field :ref,             :any, virtual: true
    field :socket,          :any, virtual: true
    field :effects,         :map, virtual: true, default: %{}
    field :last_effect_key, :integer, virtual: true, default: 0
    field :hp,              :float, virtual: true, default: 1.0
    field :mana,            :float, virtual: true, default: 1.0
    field :spells,          :map, virtual: true, default: %{}
    field :inventory,       :any, virtual: true, default: []
    field :equipment,       :any, virtual: true, default: []
    field :spell_shift,     :float, virtual: true
    field :attack_target,   :any, virtual: true
    field :strength, :integer, virtual: true
    field :agility, :integer, virtual: true
    field :intellect, :integer, virtual: true
    field :willpower, :integer, virtual: true
    field :health, :integer, virtual: true
    field :charm, :integer, virtual: true

    belongs_to :room, Room
    belongs_to :class, ApathyDrive.Class

    has_many :characters_items, ApathyDrive.EntityItem

    timestamps
  end

  @doc """
  Creates a changeset based on the `model` and `params`.

  If `params` are nil, an invalid changeset is returned
  with no validation performed.
  """
  def changeset(character, params \\ %{}) do
    character
    |> cast(params, ~w(name race_id class_id gender), ~w())
    |> validate_inclusion(:class_id, ApathyDrive.Class.ids)
    |> validate_inclusion(:race_id, ApathyDrive.Race.ids)
    |> validate_inclusion(:gender, ["male", "female"])
    |> validate_format(:name, ~r/^[a-zA-Z]+$/)
    |> unique_constraint(:name, name: :characters_lower_name_index, on: Repo)
    |> validate_length(:name, min: 1, max: 12)
  end

  def sign_up_changeset(character, params \\ %{}) do
    character
    |> cast(params, ~w(email password), [])
    |> validate_format(:email, ~r/@/)
    |> validate_length(:email, min: 3, max: 100)
    |> validate_length(:password, min: 6)
    |> unique_constraint(:email, name: :characters_lower_email_index, on: Repo)
    |> validate_confirmation(:password)
  end

  def load_spells(%Character{class_id: class_id} = character) do
    classes_spells =
      ApathyDrive.ClassSpell
      |> Ecto.Query.where(class_id: ^class_id)
      |> Ecto.Query.preload([:spell])
      |> Repo.all

    spells =
      Enum.reduce(classes_spells, %{}, fn
        %{level: level, spell: %Spell{id: id} = spell}, spells ->
          spell =
            put_in(spell.abilities, EntityAbility.load_abilities("spells", id))
            |> Map.put(:level, level)
          Map.put(spells, spell.command, spell)
      end)
    Map.put(character, :spells, spells)
  end

  def load_race(%Character{race_id: race_id} = character) do
    race = Repo.get(Race, race_id)

    effect =
      EntityAbility.load_abilities("races", race_id)
      |> Map.put("stack_key", "race")

    attributes =
      Map.take(race, [:strength, :agility, :intellect, :willpower, :health, :charm])

    character
    |> Map.put(:race, race.name)
    |> Map.merge(attributes)
    |> Systems.Effect.add(effect)
  end

  def load_class(%Character{class_id: class_id} = character) do
    class = Repo.get(ApathyDrive.Class, class_id)

    effect =
      EntityAbility.load_abilities("classes", class_id)
      |> Map.put("stack_key", "class")

    character
    |> Map.put(:class, class.name)
    |> Systems.Effect.add(effect)
  end

  def load_items(%Character{id: id} = character) do
    items = ApathyDrive.EntityItem.load_items("characters", id)

    character
    |> Map.put(:inventory, Enum.reject(items, &(&1.equipped)))
    |> Map.put(:equipment, Enum.filter(items, &(&1.equipped)))
  end

  def weapon(%Character{} = character) do
    character.equipment
    |> Enum.find(&(&1.worn_on in ["Weapon Hand", "Two Handed"]))
  end

  def sign_in(email, password) do
    player = Repo.get_by(Character, email: email)
    sign_in?(player, password) && player
  end

  def sign_in?(%Character{password: stored_hash}, password) do
    checkpw(password, stored_hash)
  end

  def sign_in?(nil, _password) do
    dummy_checkpw
  end

  def find_or_create_by_external_id(external_id) do
    case Repo.one from s in Character, where: s.external_id == ^external_id do
      %Character{} = character ->
        character
      nil ->
        %Character{room_id: Room.start_room_id, external_id: external_id}
        |> Repo.insert!
    end
  end

  def add_experience(%Character{level: level} = character, exp) do
    character =
      character
      |> Map.put(:experience, character.experience + exp)
      |> ApathyDrive.Level.advance

    %Character{id: character.id}
    |> Ecto.Changeset.change(%{experience: character.experience})
    |> Repo.update!

    if character.level > level do
      Mobile.send_scroll character, "<p>You ascend to level #{character.level}!"

      %Character{id: character.id}
      |> Ecto.Changeset.change(%{level: character.level})
      |> Repo.update!
    end

    if character.level < level do
      Mobile.send_scroll character, "<p>You fall to level #{character.level}!"

      %Character{id: character.id}
      |> Ecto.Changeset.change(%{level: character.level})
      |> Repo.update!
    end
    character
  end

  def prompt(%Character{level: level, hp: hp_percent, mana: mana_percent} = character) do
    max_hp = Mobile.max_hp_at_level(character, level)
    max_mana = Mobile.max_mana_at_level(character, level)
    hp = trunc(max_hp * hp_percent)
    mana = trunc(max_mana * mana_percent)

    if max_mana > 0 do
      "[HP=<span class='#{hp_prompt_color(hp_percent)}'>#{hp}</span>/MA=#{mana}]:"
    else
      "[HP=<span class='#{hp_prompt_color(hp_percent)}'>#{hp}</span>]:"
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

  def score_data(%Character{} = character) do
    effects =
      character.effects
      |> Map.values
      |> Enum.filter(&(Map.has_key?(&1, "StatusMessage")))
      |> Enum.map(&(&1["StatusMessage"]))

    %{
      name: character.name,
      class: character.class,
      race: character.race,
      level: character.level,
      experience: character.experience,
      perception: Mobile.perception_at_level(character, character.level),
      accuracy: Mobile.accuracy_at_level(character, character.level),
      spellcasting: Mobile.spellcasting_at_level(character, character.level),
      crits: Mobile.crits_at_level(character, character.level),
      dodge: Mobile.dodge_at_level(character, character.level),
      stealth: Mobile.stealth_at_level(character, character.level),
      tracking: Mobile.tracking_at_level(character, character.level),
      physical_damage: Mobile.physical_damage_at_level(character, character.level),
      physical_resistance: Mobile.physical_resistance_at_level(character, character.level),
      magical_damage: Mobile.magical_damage_at_level(character, character.level),
      magical_resistance: Mobile.magical_resistance_at_level(character, character.level),
      hp: hp_at_level(character, character.level),
      max_hp: Mobile.max_hp_at_level(character, character.level),
      mana: mana_at_level(character, character.level),
      max_mana: Mobile.max_mana_at_level(character, character.level),
      strength: Mobile.attribute_at_level(character, :strength, character.level),
      agility: Mobile.attribute_at_level(character, :agility, character.level),
      intellect: Mobile.attribute_at_level(character, :intellect, character.level),
      willpower: Mobile.attribute_at_level(character, :willpower, character.level),
      health: Mobile.attribute_at_level(character, :health, character.level),
      charm: Mobile.attribute_at_level(character, :charm, character.level),
      effects: effects
    }
  end

  defimpl ApathyDrive.Mobile, for: Character do

    def ability_value(character, ability) do
      Systems.Effect.effect_bonus(character, ability)
    end

    def accuracy_at_level(character, level) do
      agi = attribute_at_level(character, :agility, level)
      cha = attribute_at_level(character, :charm, level)
      agi = agi + (cha / 10)
      modifier = ability_value(character, "Accuracy")
      trunc(agi * (1 + (modifier / 100)))
    end

    def attribute_at_level(%Character{} = character, attribute, level) do
      from_race = Map.get(character, attribute)

      from_race = from_race + ((from_race / 10) * (level - 1))

      from_equipment =
        character.equipment
        |> Enum.reduce(0, fn %Item{} = item, total ->
             total + Item.attribute_for_character(item, character, attribute)
           end)

      trunc(from_race + from_equipment)
    end

    def attack_interval(character) do
      trunc(round_length_in_ms(character) / attacks_per_round(character))
    end

    def attack_spell(character) do
      case Character.weapon(character) do
        nil ->
          %Spell{
            kind: "attack",
            mana: 0,
            user_message: "You punch {{target}} for {{amount}} damage!",
            target_message: "{{user}} punches you for {{amount}} damage!",
            spectator_message: "{{user}} punches {{target}} for {{amount}} damage!",
            ignores_round_cooldown?: true,
            abilities: %{
              "PhysicalDamage" => 100 / attacks_per_round(character),
              "Dodgeable" => true,
              "DodgeUserMessage" => "You throw a punch at {{target}}, but they dodge!",
              "DodgeTargetMessage" => "{{user}} throws a punch at you, but you dodge!",
              "DodgeSpectatorMessage" => "{{user}} throws a punch at {{target}}, but they dodge!"
            }
          }
        %Item{name: name, hit_verbs: hit_verbs, miss_verbs: [singular_miss, plural_miss]} ->
          [singular_hit, plural_hit] = Enum.random(hit_verbs)
          %Spell{
            kind: "attack",
            mana: 0,
            user_message: "You #{singular_hit} {{target}} with your #{name} for {{amount}} damage!",
            target_message: "{{user}} #{plural_hit} you with their #{name} for {{amount}} damage!",
            spectator_message: "{{user}} #{plural_hit} {{target}} with their #{name} for {{amount}} damage!",
            ignores_round_cooldown?: true,
            abilities: %{
              "PhysicalDamage" => 100 / attacks_per_round(character),
              "Dodgeable" => true,
              "DodgeUserMessage" => "You #{singular_miss} {{target}} with your #{name}, but they dodge!",
              "DodgeTargetMessage" => "{{user}} #{plural_miss} you with their #{name}, but you dodge!",
              "DodgeSpectatorMessage" => "{{user}} #{plural_miss} {{target}} with their #{name}, but they dodge!"
            }
          }
      end
    end

    def attacks_per_round(character) do
      case Character.weapon(character) do
        nil ->
          4
        %Item{worn_on: "Weapon Hand", grade: "Basic"} ->
          4
        %Item{worn_on: "Two Handed", grade: "Basic"} ->
          3
        %Item{worn_on: "Weapon Hand", grade: "Bladed"} ->
          3
        %Item{worn_on: "Two Handed", grade: "Bladed"} ->
          2
        %Item{worn_on: "Weapon Hand", grade: "Blunt"} ->
          2
        %Item{worn_on: "Two Handed", grade: "Blunt"} ->
          1
      end
    end

    def caster_level(%Character{level: caster_level}, %{} = _target), do: caster_level

    def confused(%Character{effects: effects} = character, %Room{} = room) do
      effects
      |> Map.values
      |> Enum.find(fn(effect) ->
           Map.has_key?(effect, "confused") && (effect["confused"] >= :rand.uniform(100))
         end)
      |> confused(character, room)
    end
    def confused(nil, %Character{}, %Room{}), do: false
    def confused(%{"confusion_message" => %{"user" => user_message} = message}, %Character{} = character, %Room{} = room) do
      Mobile.send_scroll(character, user_message)
      if message["spectator"], do: Room.send_scroll(room, "#{Text.interpolate(message["spectator"], %{"user" => character})}", [character])
      true
    end
    def confused(%{}, %Character{} = character, %Room{} = room) do
      send_scroll(character, "<p><span class='cyan'>You fumble in confusion!</span></p>")
      Room.send_scroll(room, "<p><span class='cyan'>#{Text.interpolate("{{user}} fumbles in confusion!</span></p>", %{"user" => character})}</span></p>", [character])
      true
    end

    def crits_at_level(character, level) do
      int = attribute_at_level(character, :intellect, level)
      cha = attribute_at_level(character, :charm, level)
      int = int + (cha / 10)
      modifier = ability_value(character, "Crits")
      trunc(int * (1 + (modifier / 100)))
    end

    def die(character, room) do
      character =
        character
        |> Mobile.send_scroll("<p><span class='red'>You have died.</span></p>")
        |> Map.put(:hp, 1.0)
        |> Map.put(:mana, 1.0)
        |> Map.put(:effects, %{})
        |> Map.put(:timers, %{})
        |> Mobile.update_prompt

      Room.start_room_id
      |> RoomServer.find
      |> RoomServer.mobile_entered(character)

      put_in(room.mobiles, Map.delete(room.mobiles, character.ref))
      |> Room.send_scroll("<p><span class='red'>#{character.name} has died.</span></p>")
    end

    def dodge_at_level(character, level) do
      agi = attribute_at_level(character, :agility, level)
      cha = attribute_at_level(character, :charm, level)
      agi = agi + (cha / 10)
      modifier = ability_value(character, "Dodge")
      trunc(agi * (1 + (modifier / 100)))
    end

    def enough_mana_for_spell?(character, %Spell{} =  spell) do
      mana = Character.mana_at_level(character, character.level)
      cost = Spell.mana_cost_at_level(spell, character.level)

      mana >= cost
    end

    def enter_message(%Character{name: name}) do
      "<p><span class='yellow'>#{name}</span><span class='dark-green'> walks in from {{direction}}.</span></p>"
    end

    def exit_message(%Character{name: name}) do
      "<p><span class='yellow'>#{name}</span><span class='dark-green'> walks off {{direction}}.</span></p>"
    end

    def has_ability?(%Character{} = character, ability_name) do
      # TODO: check abilities from race, class, and spell effects
      false
    end

    def held(%{effects: effects} = mobile) do
      effects
      |> Map.values
      |> Enum.find(fn(effect) ->
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
    def hp_description(%Character{hp: hp}), do: "very critically wounded"

    def look_name(%Character{name: name}) do
      "<span class='dark-cyan'>#{name}</span>"
    end

    def magical_damage_at_level(character, level) do
      damage = attribute_at_level(character, :intellect, level)
      weapon_bonus =
        case Character.weapon(character) do
          %Item{worn_on: "Two Handed"} ->
            10
          _ ->
            0
          end

      modifier = weapon_bonus + ability_value(character, "ModifyDamage") + ability_value(character, "ModifyMagicalDamage")
      trunc(damage * (1 + (modifier / 100)))
    end

    def magical_resistance_at_level(character, level) do
      resist = attribute_at_level(character, :willpower, level)
      mr =
        character.equipment
        |> Enum.reduce(0, fn
             %Item{grade: "Cloth"}, total ->
               total + 5
             %Item{grade: "Leather"}, total ->
               total + 4
             %Item{grade: "Chain"}, total ->
               total + 3
             %Item{grade: "Scale"}, total ->
               total + 2
             %Item{grade: "Plate"}, total ->
               total + 1
             _, total ->
               total
           end)
      modifier = mr + ability_value(character, "MagicalResist")
      trunc(resist * (modifier / 100))
    end

    def max_hp_at_level(mobile, level) do
      base = trunc(ability_value(mobile, "HPPerHealth") * attribute_at_level(mobile, :health, level))
      modifier = ability_value(mobile, "MaxHP")
      trunc(base * (1 + (modifier / 100)))
    end

    def max_mana_at_level(mobile, level) do
      base = trunc(ability_value(mobile, "ManaPerIntellect") * attribute_at_level(mobile, :intellect, level))
      modifier = ability_value(mobile, "MaxMana")
      trunc(base * (1 + (modifier / 100)))
    end

    def party_refs(character, _room) do
      [character.refs]
    end

    def perception_at_level(character, level) do
      int = attribute_at_level(character, :intellect, level)
      cha = attribute_at_level(character, :charm, level)
      int = int + (cha / 10)
      modifier = ability_value(character, "Perception")
      trunc(int * (1 + (modifier / 100)))
    end

    def physical_damage_at_level(character, level) do
      damage = attribute_at_level(character, :strength, level)
      weapon_bonus =
        case Character.weapon(character) do
          %Item{worn_on: "Two Handed"} ->
            10
          _ ->
            0
          end

      modifier = weapon_bonus + ability_value(character, "ModifyDamage") + ability_value(character, "ModifyPhysicalDamage")
      trunc(damage * (1 + (modifier / 100)))
    end

    def physical_resistance_at_level(character, level) do
      resist = attribute_at_level(character, :strength, level)
      ac =
        character.equipment
        |> Enum.reduce(0, fn
             %Item{grade: "Cloth"}, total ->
               total + 1
             %Item{grade: "Leather"}, total ->
               total + 2
             %Item{grade: "Chain"}, total ->
               total + 3
             %Item{grade: "Scale"}, total ->
               total + 4
             %Item{grade: "Plate"}, total ->
               total + 5
             _, total ->
               total
           end)
      modifier = ac + ability_value(character, "AC")
      trunc(resist * (modifier / 100))
    end

    def regenerate_hp_and_mana(%Character{hp: hp, mana: mana} = character, room) do
      max_hp = max_hp_at_level(character, character.level)
      max_mana = max_mana_at_level(character, character.level)

      base_regen_per_round = attribute_at_level(character, :willpower, character.level) / 5

      hp_regen_percentage_per_round = base_regen_per_round * (1 + ability_value(character, "HPRegen") / 100) / max_hp
      mana_regen_percentage_per_round =
        if max_mana > 0 do
          base_regen_per_round * (1 + ability_value(character, "ManaRegen") / 100) / max_mana
        else
          0
        end

      character
      |> shift_hp(hp_regen_percentage_per_round, room)
      |> Map.put(:mana, min(mana + mana_regen_percentage_per_round, 1.0))
      |> TimerManager.send_after({:regen, round_length_in_ms(character), {:regen, character.ref}})
      |> update_prompt()
    end

    def round_length_in_ms(character) do
      base = 4000 - attribute_at_level(character, :agility, character.level)

      speed_mods =
        character.effects
        |> Map.values
        |> Enum.filter(&(Map.has_key?(&1, "Speed")))
        |> Enum.map(&(Map.get(&1, "Speed")))

      count = length(speed_mods)

      if count > 0 do
        trunc(base * (Enum.sum(speed_mods) / count / 100))
      else
        base
      end
    end

    def send_scroll(%Character{socket: socket} = character, html) do
      send(socket, {:scroll, html})
      character
    end

    def set_room_id(%Character{socket: socket, monitor_ref: monitor_ref} = character, room_id) do
      Process.demonitor(monitor_ref)

      send(character.socket, {:update_room, room_id})

      character
      |> Map.put(:room_id, room_id)
      |> Map.put(:monitor_ref, Process.monitor(socket))
      |> Repo.save!
    end

    def shift_hp(character, percentage, room) do
      hp_description = hp_description(character)
      character = update_in(character.hp, &(min(1.0, &1 + percentage)))
      updated_hp_description = hp_description(character)

      if character.hp > 0 and hp_description != updated_hp_description do
        Room.send_scroll(room, "<p>#{look_name(character)} is #{updated_hp_description}.</p>", [character])
      end

      character
    end

    def silenced(%Character{effects: effects} = character, %Room{} = room) do
      effects
      |> Map.values
      |> Enum.find(fn(effect) ->
           Map.has_key?(effect, "Silence")
         end)
      |> silenced(character, room)
    end
    def silenced(nil, %Character{}, %Room{}), do: false
    def silenced(%{}, %Character{} = character, %Room{} = room) do
      Mobile.send_scroll(character, "<p><span class='cyan'>You are silenced!</span></p>")
      true
    end

    def spellcasting_at_level(character, level) do
      will = attribute_at_level(character, :willpower, level)
      cha = attribute_at_level(character, :charm, level)
      will = will + (cha / 10)
      modifier = ability_value(character, "Spellcasting")
      trunc(will * (1 + (modifier / 100)))
    end

    def spells_at_level(%Character{spells: spells}, level) do
      spells
      |> Map.values
      |> Enum.filter(& &1.level <= level)
      |> Enum.sort_by(& &1.level)
    end

    def stealth_at_level(character, level) do
      agi = attribute_at_level(character, :agility, level)
      cha = attribute_at_level(character, :charm, level)
      agi = agi + (cha / 10)
      modifier = ability_value(character, "Stealth")
      trunc(agi * (modifier / 100))
    end

    def subtract_mana(character, spell) do
      cost = Spell.mana_cost_at_level(spell, character.level)
      percentage = cost / Mobile.max_mana_at_level(character, character.level)
      update_in(character.mana, &(max(0, &1 - percentage)))
    end

    def target_level(%Character{level: _caster_level}, %Character{level: target_level}), do: target_level
    def target_level(%Character{level: caster_level}, %{level: _target_level}), do: caster_level

    def tracking_at_level(character, level) do
      perception = perception_at_level(character, level)
      modifier = ability_value(character, "Tracking")
      trunc(perception * (modifier / 100))
    end

    def update_prompt(%Character{socket: socket} = character) do
      send(socket, {:update_prompt, Character.prompt(character)})
      character
    end
  end

end
