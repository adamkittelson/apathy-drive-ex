defmodule ApathyDrive.Monster do
  use Ecto.Schema
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Character, Item, Mobile, Monster, Room, RoomMonster, Spell, Text}

  require Logger

  schema "monsters" do
    field :name,             :string
    field :gender,           :string
    field :grade,            :string
    field :hostile,          :boolean
    field :movement,         :string
    field :chance_to_follow, :integer
    field :description,      :string
    field :enter_message,    :string
    field :exit_message,     :string
    field :death_message,    :string
    field :adjectives,       ApathyDrive.JSONB
    field :next_spawn_at,    :integer

    field :hp,          :float, virtual: true, default: 1.0
    field :mana,        :float, virtual: true, default: 1.0
    field :ref,         :any, virtual: true
    field :timers,      :map, virtual: true, default: %{}
    field :effects,     :map, virtual: true, default: %{}
    field :last_effect_key, :integer, virtual: true, default: 0
    field :spells,      :map, virtual: true, default: %{}
    field :strength,    :integer, virtual: true
    field :agility,     :integer, virtual: true
    field :intellect,   :integer, virtual: true
    field :willpower,   :integer, virtual: true
    field :health,      :integer, virtual: true
    field :charm,       :integer, virtual: true
    field :room_monster_id, :integer, virtual: true
    field :room_id,     :integer, virtual: true
    field :level,       :integer, virtual: true
    field :spawned_at,  :integer, virtual: true
    field :spell_shift, :float, virtual: true

    timestamps
  end

  @grades %{
    "weak" => 50,
    "normal" => 100,
    "strong" => 150,
    "boss" => 200
  }

  def from_room_monster(%RoomMonster{id: nil, room_id: room_id, monster_id: monster_id} = rm) do
    now =
      DateTime.utc_now
      |> DateTime.to_unix

    monster =
      Repo.get(Monster, monster_id)
      |> Map.put(:room_id, room_id)
      |> Map.put(:level, rm.level)
      |> Map.put(:spawned_at, rm.spawned_at)

    if spawnable?(monster, now) do
      monster
      |> Map.put(:ref, make_ref())
      |> generate_monster_attributes()
    end
  end
  def from_room_monster(%RoomMonster{id: id, monster_id: monster_id} = rm) do
    monster = Repo.get(Monster, monster_id)

    attributes = Map.take(rm, [:strength, :agility, :intellect,
                               :willpower, :health, :charm, :name])

    monster
    |> Map.merge(attributes)
    |> Map.put(:room_monster_id, id)
    |> Map.put(:ref, make_ref())
    |> Map.put(:level, rm.level)
  end

  def spawnable?(%Monster{grade: "boss", next_spawn_at: time}, now) when not is_nil(time) and time > now, do: false
  def spawnable?(%Monster{}, _now), do: true

  def generate_monster_attributes(%Monster{grade: grade, level: level} = monster) do
    base = @grades[grade] * (1 + (level / 10))
    min = trunc(base * 0.75)
    max = trunc(base * 1.25)

    room_monster =
      %RoomMonster{
        level: level,
        room_id: monster.room_id,
        monster_id: monster.id,
        spawned_at: monster.spawned_at,
        name: name_with_adjective(monster.name, monster.adjectives)
      }

    room_monster =
      [:strength, :agility, :intellect, :willpower, :health, :charm]
      |> Enum.shuffle
      |> Enum.chunk(2)
      |> Enum.reduce(room_monster, fn
           [attribute_1, attribute_2], rm ->
             roll = Enum.random(min..max)
             rm
             |> Map.put(attribute_1, roll)
             |> Map.put(attribute_2, max - roll + min)
           end)
      |> Repo.insert!

    if grade == "boss" do
      # set to 100 years from now, e.g. don't spawn this monster again
      # updated to something closer to now when the monster is killed
      spawn_at =
        DateTime.utc_now.year
        |> update_in(&(&1 + 100))
        |> DateTime.to_unix

      %Monster{id: monster.id}
      |> Ecto.Changeset.change(next_spawn_at: spawn_at)
      |> Repo.update!
    end

    monster
    |> Map.merge(Map.take(room_monster, [:strength, :agility, :intellect, :willpower, :health, :charm, :name]))
    |> Map.put(:room_monster_id, room_monster.id)
  end
  def generate_monster_attributes(%Monster{} = monster), do: monster

  def generate_loot_for_character(%Monster{} = monster, %Character{} = character) do
    rarity = random_loot_rarity(monster, character.pity_modifier)

    if rarity do
      power = Item.power_at_level(rarity, character.level)

      slots =
        case Item.slots_below_power(character, power) do
          [] ->
            Item.slots
          slots ->
            slots
        end

      slot = Enum.random(slots)

      item_id = Item.random_item_id_for_slot_and_rarity(character, slot, rarity)

      item =
        Item
        |> Repo.get(item_id)
        |> Item.generate_for_character!(character, :looted)

      character =
        if rarity == "legendary" do
          character = put_in(character.pity_modifier, 0)

          %Character{id: character.id}
          |> Ecto.Changeset.change(%{pity_modifier: character.pity_modifier})
          |> Repo.update!

          character
        else
          character
        end

      character
      |> Mobile.send_scroll("<p>You receive #{Item.colored_name(item)}!</p>")
      |> Character.load_items
    else
      character = update_in(character.pity_modifier, &(&1 + Monster.pity_bonus(monster)))

      %Character{id: character.id}
      |> Ecto.Changeset.change(%{pity_modifier: character.pity_modifier})
      |> Repo.update!

      character
    end
  end

  def pity_bonus(%Monster{grade: "weak"}), do: 10
  def pity_bonus(%Monster{grade: "normal"}), do: 100
  def pity_bonus(%Monster{grade: "strong"}), do: 1_000
  def pity_bonus(%Monster{grade: "boss"}), do: 10_000

  def random_loot_rarity(%Monster{grade: "weak"} = monster, pity_modifier) do
    case :rand.uniform(1_000_000 - pity_modifier) do
      roll when roll <= 10 ->
        "legendary"
      roll when roll <= 100 ->
        "epic"
      roll when roll <= 1000 ->
        "rare"
      roll when roll <= 10_000 ->
        "uncommon"
      roll when roll <= 100_000 ->
        "common"
      _ ->
        nil
    end
  end
  def random_loot_rarity(%Monster{grade: "normal"} = monster, pity_modifier) do
    case :rand.uniform(1_000_000 - pity_modifier) do
      roll when roll <= 100 ->
        "legendary"
      roll when roll <= 1000 ->
        "epic"
      roll when roll <= 10_000 ->
        "rare"
      roll when roll <= 100_000 ->
        "uncommon"
      _ ->
        "common"
    end
  end
  def random_loot_rarity(%Monster{grade: "strong"} = monster, pity_modifier) do
    case :rand.uniform(1_000_000 - pity_modifier) do
      roll when roll <= 1000 ->
        "legendary"
      roll when roll <= 10_000 ->
        "epic"
      roll when roll <= 100_000 ->
        "rare"
      _ ->
        "uncommon"
    end
  end
  def random_loot_rarity(%Monster{grade: "boss"} = monster, pity_modifier) do
    case :rand.uniform(1_000_000 - pity_modifier) do
      roll when roll <= 10_000 ->
        "legendary"
      roll when roll <= 100_000 ->
        "epic"
      _ ->
        "rare"
    end
  end

  def name_with_adjective(name, nil), do: name
  def name_with_adjective(name, []),  do: name
  def name_with_adjective(name, adjectives) do
    adjective = adjectives
                |> Enum.random

    "#{adjective} #{name}"
  end

  defimpl ApathyDrive.Mobile, for: Monster do

    def ability_value(monster, ability) do
      # TODO: add race and class ability values
      Systems.Effect.effect_bonus(monster, ability)
    end

    def accuracy_at_level(monster, level) do
      agi = attribute_at_level(monster, :agility, level)
      cha = attribute_at_level(monster, :charm, level)
      agi = agi + (cha / 10)
      modifier = ability_value(monster, "Accuracy")
      trunc(agi * (1 + (modifier / 100)))
    end

    def attribute_at_level(%Monster{} = monster, attribute, level) do
      base = Map.get(monster, attribute)

      trunc(base + ((base / 10) * (level - 1)))
    end

    def attack_interval(monster) do
      trunc(round_length_in_ms(monster) / attacks_per_round(monster))
    end

    def attack_spell(monster) do
      case Monster.weapon(monster) do
        nil ->
          %Spell{
            kind: "attack",
            mana: 0,
            user_message: "You punch {{target}} for {{amount}} damage!",
            target_message: "{{user}} punches you for {{amount}} damage!",
            spectator_message: "{{user}} punches {{target}} for {{amount}} damage!",
            ignores_round_cooldown?: true,
            abilities: %{
              "PhysicalDamage" => 100 / attacks_per_round(monster),
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
              "PhysicalDamage" => 100 / attacks_per_round(monster),
              "Dodgeable" => true,
              "DodgeUserMessage" => "You #{singular_miss} {{target}} with your #{name}, but they dodge!",
              "DodgeTargetMessage" => "{{user}} #{plural_miss} you with their #{name}, but you dodge!",
              "DodgeSpectatorMessage" => "{{user}} #{plural_miss} {{target}} with their #{name}, but they dodge!"
            }
          }
      end
    end

    def attacks_per_round(monster) do
      case Monster.weapon(monster) do
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

    def caster_level(%Monster{}, %Character{level: level} = _target), do: level

    def colored_name(%Monster{name: name} = monster, %Character{} = observer) do
      level = target_level(monster, observer)
      monster_power = Mobile.power_at_level(monster, level)
      observer_power = Mobile.power_at_level(observer, level)

      color =
        cond do
          monster_power < (observer_power * 0.75) ->
            "darkgrey"
          monster_power < (observer_power * 1.5) ->
            "teal"
          monster_power < (observer_power * 3.0) ->
            "#0070ff"
          monster_power < (observer_power * 6.0) ->
            "#a335ee"
          :else ->
            "#ff8000"
        end
      "<span style='color: #{color};'>#{name}</span>"
    end

    def confused(%Monster{effects: effects} = monster, %Room{} = room) do
      effects
      |> Map.values
      |> Enum.find(fn(effect) ->
           Map.has_key?(effect, "confused") && (effect["confused"] >= :rand.uniform(100))
         end)
      |> confused(monster, room)
    end
    def confused(nil, %Monster{}, %Room{}), do: false
    def confused(%{"confusion_message" => %{"user" => user_message} = message}, %Monster{} = monster, %Room{} = room) do
      Mobile.send_scroll(monster, user_message)
      if message["spectator"], do: Room.send_scroll(room, "#{Text.interpolate(message["spectator"], %{"user" => monster})}", [monster])
      true
    end
    def confused(%{}, %Monster{} = monster, %Room{} = room) do
      send_scroll(monster, "<p><span class='cyan'>You fumble in confusion!</span></p>")
      Room.send_scroll(room, "<p><span class='cyan'>#{Text.interpolate("{{user}} fumbles in confusion!</span></p>", %{"user" => monster})}</span></p>", [monster])
      true
    end

    def crits_at_level(monster, level) do
      int = attribute_at_level(monster, :intellect, level)
      cha = attribute_at_level(monster, :charm, level)
      int = int + (cha / 10)
      modifier = ability_value(monster, "Crits")
      trunc(int * (1 + (modifier / 100)))
    end

    def description(monster, _observer) do
      monster.description
    end

    def die(monster, room) do
      room =
        Enum.reduce(room.mobiles, room, fn
          {ref, %Character{}}, updated_room ->
            Room.update_mobile(updated_room, ref, fn(character) ->
              level = Mobile.target_level(character, monster)

              exp =
                [:strength, :agility, :intellect, :willpower, :health, :charm]
                |> Enum.map(&Mobile.attribute_at_level(monster, &1, level))
                |> Enum.reduce(0, &(&1 + &2))
                |> div(10)

              message =
                monster.death_message
                |> Text.interpolate(%{"name" => monster.name})
                |> Text.capitalize_first

              Mobile.send_scroll(character, "<p>#{message}</p>")

              Mobile.send_scroll(character, "<p>You gain #{exp} experience.</p>")


              character = Character.add_experience(character, exp)

              Monster.generate_loot_for_character(monster, character)
            end)
          _, updated_room ->
            updated_room
        end)

      ApathyDrive.Repo.delete!(%RoomMonster{id: monster.room_monster_id})

      if monster.grade == "boss" do
        # set to 6 hours from now so the boss won't respawn until then
        spawn_at =
          DateTime.utc_now.year
          |> DateTime.to_unix
          |> Kernel.+(6 * 60 * 60)

        %Monster{id: monster.id}
        |> Ecto.Changeset.change(next_spawn_at: spawn_at)
        |> Repo.update!
      end

      put_in(room.mobiles, Map.delete(room.mobiles, monster.ref))
    end

    def dodge_at_level(monster, level) do
      agi = attribute_at_level(monster, :agility, level)
      cha = attribute_at_level(monster, :charm, level)
      agi = agi + (cha / 10)
      modifier = ability_value(monster, "Dodge")
      trunc(agi * (1 + (modifier / 100)))
    end

    def enough_mana_for_spell?(monster, %Spell{} =  spell) do
      mana = Monster.mana_at_level(monster, monster.level)
      cost = Spell.mana_cost_at_level(spell, monster.level)

      mana >= cost
    end

    def enter_message(%Monster{name: name}) do
      "<p><span class='yellow'>#{name}</span><span class='dark-green'> walks in from {{direction}}.</span></p>"
    end

    def exit_message(%Monster{name: name}) do
      "<p><span class='yellow'>#{name}</span><span class='dark-green'> walks off {{direction}}.</span></p>"
    end

    def has_ability?(%Monster{} = monster, ability_name) do
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

    def hp_description(%Monster{hp: hp}) when hp >= 1.0, do: "unwounded"
    def hp_description(%Monster{hp: hp}) when hp >= 0.9, do: "slightly wounded"
    def hp_description(%Monster{hp: hp}) when hp >= 0.6, do: "moderately wounded"
    def hp_description(%Monster{hp: hp}) when hp >= 0.4, do: "heavily wounded"
    def hp_description(%Monster{hp: hp}) when hp >= 0.2, do: "severely wounded"
    def hp_description(%Monster{hp: hp}) when hp >= 0.1, do: "critically wounded"
    def hp_description(%Monster{hp: hp}), do: "very critically wounded"

    def look_name(%Monster{name: name}) do
      "<span class='dark-cyan'>#{name}</span>"
    end

    def magical_damage_at_level(monster, level) do
      damage = attribute_at_level(monster, :intellect, level)
      weapon_bonus =
        case Monster.weapon(monster) do
          %Item{worn_on: "Two Handed"} ->
            10
          _ ->
            0
          end

      modifier = weapon_bonus + ability_value(monster, "ModifyDamage") + ability_value(monster, "ModifyMagicalDamage")
      trunc(damage * (1 + (modifier / 100)))
    end

    def magical_resistance_at_level(monster, level) do
      resist = attribute_at_level(monster, :willpower, level)
      modifier = ability_value(monster, "MagicalResist")
      trunc(resist * (modifier / 100))
    end

    def max_hp_at_level(%Monster{grade: grade} = monster, level) do
      base =
        case grade do
          "weak" ->
            4
          "normal" ->
            8
          "strong" ->
            16
          "boss" ->
            32
        end
      base = trunc((base + ability_value(monster, "HPPerHealth")) * attribute_at_level(monster, :health, level))
      modifier = ability_value(monster, "MaxHP")
      trunc(base * (1 + (modifier / 100)))
    end

    def max_mana_at_level(mobile, level) do
      base = trunc(5 * attribute_at_level(mobile, :intellect, level))
      modifier = ability_value(mobile, "MaxMana")
      trunc(base * (1 + (modifier / 100)))
    end

    def party_refs(monster, _room) do
      [monster.refs]
    end

    def perception_at_level(monster, level) do
      int = attribute_at_level(monster, :intellect, level)
      cha = attribute_at_level(monster, :charm, level)
      int = int + (cha / 10)
      modifier = ability_value(monster, "Perception")
      trunc(int * (1 + (modifier / 100)))
    end

    def physical_damage_at_level(monster, level) do
      damage = attribute_at_level(monster, :strength, level)
      weapon_bonus =
        case Monster.weapon(monster) do
          %Item{worn_on: "Two Handed"} ->
            10
          _ ->
            0
          end

      modifier = weapon_bonus + ability_value(monster, "ModifyDamage") + ability_value(monster, "ModifyPhysicalDamage")
      trunc(damage * (1 + (modifier / 100)))
    end

    def physical_resistance_at_level(monster, level) do
      resist = attribute_at_level(monster, :strength, level)
      modifier = ability_value(monster, "AC")
      trunc(resist * (modifier / 100))
    end

    def power_at_level(%Monster{} = monster, level) do
      [:strength, :agility, :intellect, :willpower, :health, :charm]
      |> Enum.reduce(0, & &2 + Mobile.attribute_at_level(monster, &1, level))
    end

    def regenerate_hp_and_mana(%Monster{hp: hp, mana: mana} = monster, room) do
      max_hp = max_hp_at_level(monster, monster.level)
      max_mana = max_mana_at_level(monster, monster.level)

      base_regen_per_round = attribute_at_level(monster, :willpower, monster.level) / 5

      hp_regen_percentage_per_round = base_regen_per_round * (1 + ability_value(monster, "HPRegen")) / max_hp
      mana_regen_percentage_per_round = base_regen_per_round * (1 + ability_value(monster, "ManaRegen")) / max_mana

      monster
      |> shift_hp(hp_regen_percentage_per_round, room)
      |> Map.put(:mana, min(mana + mana_regen_percentage_per_round, 1.0))
      |> TimerManager.send_after({:regen, round_length_in_ms(monster), {:regen, monster.ref}})
      |> update_prompt()
    end

    def round_length_in_ms(monster) do
      base = 4000 - attribute_at_level(monster, :agility, monster.level)

      speed_mods =
        monster.effects
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

    def send_scroll(%Monster{} = monster, _html) do
      monster
    end

    def set_room_id(%Monster{} = monster, room_id) do
      monster
      |> Map.put(:room_id, room_id)
      # TODO: update rooms_monsters record instead
      #|> Repo.save!
    end

    def shift_hp(monster, percentage, room) do
      hp_description = hp_description(monster)
      monster = update_in(monster.hp, &(min(1.0, &1 + percentage)))
      updated_hp_description = hp_description(monster)

      if monster.hp > 0 and hp_description != updated_hp_description do
        room.mobiles
        |> Map.values
        |> List.delete(monster)
        |> Enum.each(fn
             %Character{} = observer ->
               Mobile.send_scroll(observer, "<p>#{Mobile.colored_name(monster, observer)} is #{updated_hp_description}.</p>")
             _ ->
               :noop
           end)
      end

      monster
    end

    def silenced(%Monster{effects: effects} = monster, %Room{} = room) do
      effects
      |> Map.values
      |> Enum.find(fn(effect) ->
           Map.has_key?(effect, "Silence")
         end)
      |> silenced(monster, room)
    end
    def silenced(nil, %Monster{}, %Room{}), do: false
    def silenced(%{}, %Monster{} = monster, %Room{} = room) do
      Mobile.send_scroll(monster, "<p><span class='cyan'>You are silenced!</span></p>")
      true
    end

    def spellcasting_at_level(monster, level) do
      will = attribute_at_level(monster, :willpower, level)
      cha = attribute_at_level(monster, :charm, level)
      will = will + (cha / 10)
      modifier = ability_value(monster, "Spellcasting")
      trunc(will * (1 + (modifier / 100)))
    end

    def spells_at_level(%Monster{spells: spells}, level) do
      spells
      |> Map.values
      |> Enum.filter(& &1.level <= level)
      |> Enum.sort_by(& &1.level)
    end

    def stealth_at_level(monster, level) do
      agi = attribute_at_level(monster, :agility, level)
      cha = attribute_at_level(monster, :charm, level)
      agi = agi + (cha / 10)
      modifier = ability_value(monster, "Stealth")
      trunc(agi * (modifier / 100))
    end

    def subtract_mana(monster, spell) do
      cost = Spell.mana_cost_at_level(spell, monster.level)
      percentage = cost / Mobile.max_mana_at_level(monster, monster.level)
      update_in(monster.mana, &(max(0, &1 - percentage)))
    end

    def target_level(%Monster{level: monster_level}, %Character{level: target_level}) when target_level > monster_level, do: target_level
    def target_level(%Monster{level: monster_level}, %Character{level: target_level}), do: monster_level

    def tracking_at_level(monster, level) do
      perception = perception_at_level(monster, level)
      modifier = ability_value(monster, "Tracking")
      trunc(perception * (modifier / 100))
    end

    def update_prompt(%Monster{} = monster) do
      monster
    end
  end

end
