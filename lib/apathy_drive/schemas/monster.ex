defmodule ApathyDrive.Monster do
  use Ecto.Schema
  use ApathyDriveWeb, :model

  alias ApathyDrive.{
    Ability,
    Aggression,
    AI,
    Character,
    Currency,
    Regeneration,
    Item,
    ItemInstance,
    KillCount,
    Mobile,
    Monster,
    MonsterAbility,
    MonsterItem,
    MonsterSpawning,
    MonsterTrait,
    Party,
    Room,
    RoomMonster,
    RoomServer,
    Text,
    TimerManager
  }

  require Logger

  schema "monsters" do
    field(:name, :string)
    field(:gender, :string)
    field(:grade, :string)
    field(:hostile, :boolean)
    field(:movement, :string)
    field(:chance_to_follow, :integer)
    field(:description, :string)
    field(:enter_message, :string)
    field(:exit_message, :string)
    field(:death_message, :string)
    field(:adjectives, ApathyDrive.JSONB)
    field(:questions, ApathyDrive.JSONB)
    field(:greeting, :string)
    field(:next_spawn_at, :integer)
    field(:copper, :integer, default: 0)
    field(:silver, :integer, default: 0)
    field(:gold, :integer, default: 0)
    field(:platinum, :integer, default: 0)
    field(:runic, :integer, default: 0)
    field(:experience, :integer, default: 0)
    field(:base_hp, :integer)
    field(:hp_regen, :integer)
    field(:regen_time_in_hours, :integer)
    field(:game_limit, :integer)
    field(:alignment, :string)
    field(:lawful, :boolean)

    field(:leader, :any, virtual: true)
    field(:hp, :float, virtual: true, default: 1.0)
    field(:mana, :float, virtual: true, default: 1.0)
    field(:ref, :any, virtual: true)
    field(:timers, :map, virtual: true, default: %{})
    field(:effects, :map, virtual: true, default: %{})
    field(:last_effect_key, :integer, virtual: true, default: 0)
    field(:abilities, :map, virtual: true, default: %{})
    field(:strength, :integer, virtual: true)
    field(:agility, :integer, virtual: true)
    field(:intellect, :integer, virtual: true)
    field(:willpower, :integer, virtual: true)
    field(:health, :integer, virtual: true)
    field(:charm, :integer, virtual: true)
    field(:room_monster_id, :integer, virtual: true)
    field(:room_id, :integer, virtual: true)
    field(:level, :integer, virtual: true)
    field(:spawned_at, :integer, virtual: true)
    field(:zone_spawned_at, :integer, virtual: true)
    field(:ability_shift, :float, virtual: true)
    field(:ability_special, :float, virtual: true)
    field(:energy, :integer, virtual: true, default: 1000)
    field(:max_energy, :integer, virtual: true, default: 1000)
    field(:casting, :any, virtual: true)
    field(:last_tick_at, :any, virtual: true)
    field(:last_room_id, :integer, virtual: true)
    field(:auto_heal, :boolean, virtual: true, default: true)
    field(:auto_bless, :boolean, virtual: true, default: true)
    field(:auto_curse, :boolean, virtual: true, default: true)
    field(:auto_nuke, :boolean, virtual: true, default: true)
    field(:auto_flee, :boolean, virtual: true, default: false)
    field(:drops, :any, virtual: true, default: [])
    field(:sneaking, :boolean, virtual: true, default: false)

    timestamps()

    has_many(:lairs, ApathyDrive.LairMonster)
    has_many(:lair_rooms, through: [:lairs, :room])

    has_many(:monsters_traits, ApathyDrive.MonsterTrait)
    has_many(:traits, through: [:monsters_traits, :trait])
  end

  def changeset(%Monster{} = monster, params \\ %{}) do
    monster
    |> cast(params, ~w(), ~w())
  end

  def loot_wealth_in_copper(%Monster{} = monster) do
    monster
    |> Map.put(:copper, Enum.random(0..monster.copper))
    |> Map.put(:silver, Enum.random(0..monster.silver))
    |> Map.put(:gold, Enum.random(0..monster.gold))
    |> Map.put(:platinum, Enum.random(0..monster.platinum))
    |> Map.put(:runic, Enum.random(0..monster.runic))
    |> Currency.wealth()
  end

  def enemies(%Monster{} = monster, room) do
    monster.effects
    |> Map.values()
    |> Enum.filter(&Map.has_key?(&1, "Aggro"))
    |> Enum.map(&Map.get(&1, "Aggro"))
    |> Enum.filter(&(&1 in Map.keys(room.mobiles)))
    |> Enum.reject(&room.mobiles[&1].sneaking)
  end

  def hireable?(%Monster{} = monster, character, room) do
    !(Mobile.has_ability?(monster, "NonLiving") or Mobile.has_ability?(monster, "Animal") or
        Mobile.has_ability?(monster, "Undead") or monster.hostile or
        character.ref in enemies(monster, room))
  end

  def chase_exit(monster, room, destination) do
    room
    |> AI.exits_in_area(monster)
    |> Enum.find(&(&1["destination"] == destination))
  end

  def chase(%Monster{} = monster, room, character, destination) do
    with {:aggro?, true} <- {:aggro?, Aggression.enemy?(monster, character)},
         {:follow?, true} <- {:follow?, :rand.uniform(100) < monster.chance_to_follow},
         {:exit, %{} = room_exit} <- {:exit, chase_exit(monster, room, destination)} do
      ApathyDrive.Commands.Move.execute(room, monster, room_exit, false)
    else
      _ ->
        room
    end
  end

  def from_room_monster(%RoomMonster{id: nil, room_id: room_id, monster_id: monster_id} = rm) do
    now =
      DateTime.utc_now()
      |> DateTime.to_unix()

    monster =
      Repo.get(Monster, monster_id)
      |> Map.put(:room_id, room_id)
      |> Map.put(:level, rm.level)
      |> Map.put(:spawned_at, rm.spawned_at)
      |> Map.put(:zone_spawned_at, rm.zone_spawned_at)

    if !MonsterSpawning.limit_reached?(monster) and spawnable?(monster, now) do
      ref = :crypto.hash(:md5, inspect(make_ref())) |> Base.encode16()

      monster
      |> Map.put(:ref, ref)
      |> generate_monster_attributes()
      |> load_abilities()
      |> load_traits()
      |> load_drops()
      |> Mobile.cpr()
    end
  end

  def from_room_monster(%RoomMonster{id: id, monster_id: monster_id} = rm) do
    monster = Repo.get(Monster, monster_id)

    attributes =
      Map.take(rm, [
        :strength,
        :agility,
        :intellect,
        :willpower,
        :health,
        :charm,
        :name,
        :spawned_at,
        :room_spawned_at
      ])

    ref = :crypto.hash(:md5, inspect(make_ref())) |> Base.encode16()

    monster
    |> Map.merge(attributes)
    |> Map.put(:room_monster_id, id)
    |> Map.put(:ref, ref)
    |> Map.put(:level, rm.level)
    |> load_abilities()
    |> load_traits()
    |> load_drops()
    |> Mobile.cpr()
  end

  def spawnable?(%Monster{next_spawn_at: nil}, _now), do: true
  def spawnable?(%Monster{regen_time_in_hours: nil}, _now), do: true
  def spawnable?(%Monster{next_spawn_at: time}, now), do: now >= time

  def load_traits(%Monster{id: id} = monster) do
    effect =
      MonsterTrait.load_traits(id)
      |> Map.put("stack_key", "monster")

    monster
    |> Systems.Effect.add(effect)
  end

  def load_drops(%Monster{} = monster) do
    MonsterItem.load_drops(monster)
  end

  def load_abilities(%Monster{} = monster) do
    MonsterAbility.load_abilities(monster)
  end

  def generate_monster_attributes(%Monster{level: level} = monster) do
    room_monster = %RoomMonster{
      level: level,
      room_id: monster.room_id,
      monster_id: monster.id,
      spawned_at: monster.spawned_at,
      zone_spawned_at: monster.zone_spawned_at,
      name: name_with_adjective(monster.name, monster.adjectives)
    }

    room_monster =
      [:strength, :agility, :intellect, :willpower, :health, :charm]
      |> Enum.reduce(room_monster, fn attribute, rm ->
        rm
        |> Map.put(attribute, 50)
      end)
      |> Repo.insert!()

    monster
    |> Map.merge(
      Map.take(room_monster, [:strength, :agility, :intellect, :willpower, :health, :charm, :name])
    )
    |> Map.put(:room_monster_id, room_monster.id)
  end

  def generate_monster_attributes(%Monster{} = monster), do: monster

  def generate_loot_for_character(%Monster{} = monster, %Character{} = character) do
    rarity = nil

    if rarity do
      Logger.info("spawning #{rarity} item for #{character.name}")

      item_id = 1

      if item_id do
        Item
        |> Repo.get(item_id)
        |> Item.generate_for_character!(character, :loot)
        |> case do
          %Item{instance_id: nil} = item ->
            gold =
              item
              |> Item.price()
              |> div(10)

            character
            |> Ecto.Changeset.change(%{gold: character.gold + gold})
            |> Repo.update!()
            |> Mobile.send_scroll("<p>You find #{gold} gold crowns on the body.</p>")

          %Item{instance_id: _id} = item ->
            character
            |> Mobile.send_scroll("<p>You receive #{Item.colored_name(item)}!</p>")
            |> Character.load_items()
        end
      else
        character = update_in(character.pity_modifier, &(&1 + Monster.pity_bonus(monster)))

        %Character{id: character.id}
        |> Ecto.Changeset.change(%{pity_modifier: character.pity_modifier})
        |> Repo.update!()

        character
      end
    else
      character = update_in(character.pity_modifier, &(&1 + Monster.pity_bonus(monster)))

      %Character{id: character.id}
      |> Ecto.Changeset.change(%{pity_modifier: character.pity_modifier})
      |> Repo.update!()

      character
    end
  end

  def pity_bonus(%Monster{grade: "weak"}), do: 10
  def pity_bonus(%Monster{grade: "normal"}), do: 100
  def pity_bonus(%Monster{grade: "strong"}), do: 1_000
  def pity_bonus(%Monster{grade: "boss"}), do: 10_000

  def name_with_adjective(name, nil), do: name
  def name_with_adjective(name, []), do: name

  def name_with_adjective(name, adjectives) do
    adjective =
      adjectives
      |> Enum.random()

    "#{adjective} #{name}"
  end

  def auto_attack_target(_monster, [], _room) do
    nil
  end

  # weak monsters attack enemy with lowest % hp remaining
  def auto_attack_target(%Monster{grade: "weak"}, enemies, room) do
    enemies
    |> Enum.map(&room.mobiles[&1])
    |> Enum.sort_by(& &1.hp)
    |> List.first()
    |> Map.get(:ref)
  end

  # normal monsters attack a random enemy
  def auto_attack_target(%Monster{grade: "normal"}, enemies, _room) do
    Enum.random(enemies)
  end

  # strong monsters attack enemy with highest % hp remaining
  def auto_attack_target(%Monster{}, enemies, room) do
    enemies
    |> Enum.map(&room.mobiles[&1])
    |> Enum.sort_by(& &1.hp)
    |> List.last()
    |> Map.get(:ref)
  end

  def drop_loot_for_character(%Room{} = room, %Monster{} = monster, %Character{id: id}) do
    Enum.reduce(monster.drops, room, fn %{chance: chance, item_id: item_id}, room ->
      IO.puts("item_id: #{item_id}, chance: #{chance}")

      if :rand.uniform(100) <= chance do
        %ItemInstance{
          item_id: item_id,
          room_id: room.id,
          character_id: nil,
          dropped_for_character_id: id,
          equipped: false,
          hidden: false,
          delete_at: Timex.shift(DateTime.utc_now(), hours: 1)
        }
        |> Repo.insert!()

        Room.load_items(room)
      else
        room
      end
    end)
  end

  defimpl ApathyDrive.Mobile, for: Monster do
    def ability_value(monster, ability) do
      # TODO: add race and class ability values
      Systems.Effect.effect_bonus(monster, ability)
    end

    def accuracy_at_level(monster, level, _room) do
      agi = attribute_at_level(monster, :agility, level)
      cha = attribute_at_level(monster, :charm, level)
      agi = agi + cha / 10
      modifier = ability_value(monster, "Accuracy")
      trunc(agi * (1 + modifier / 100))
    end

    def alignment(monster, _room), do: monster.alignment

    def attribute_at_level(%Monster{} = monster, attribute, level) do
      Map.get(monster, attribute) + level - 1 +
        ability_value(monster, attribute |> to_string |> String.capitalize())
    end

    def attack_ability(monster) do
      monster.abilities
      |> Map.values()
      |> Enum.filter(&(&1.kind == "auto attack"))
      |> case do
        [] ->
          nil

        attacks ->
          attacks =
            attacks
            |> Enum.sort_by(& &1.chance)

          attacks
          |> Enum.find(fn attack ->
            rand = :rand.uniform(100)

            attack.chance > rand or attack == List.last(attacks)
          end)
          |> Map.put(:kind, "attack")
          |> Map.put(:ignores_round_cooldown?, true)
      end
    end

    def auto_attack_target(%Monster{} = monster, room) do
      enemies = Monster.enemies(monster, room)

      Monster.auto_attack_target(monster, enemies, room)
    end

    def caster_level(%Monster{level: level}, %Monster{} = _target), do: level

    def caster_level(%Monster{level: level}, %{level: target_level} = _target),
      do: max(level, target_level)

    def color(%Monster{alignment: "evil"}), do: "magenta"
    def color(%Monster{alignment: "neutral"}), do: "dark-cyan"
    def color(%Monster{alignment: "good"}), do: "grey"

    def colored_name(%Monster{name: name} = monster) do
      "<span class='#{color(monster)}'>#{name}</span>"
    end

    def confused(%Monster{effects: effects} = monster, %Room{} = room) do
      effects
      |> Map.values()
      |> Enum.find(fn effect ->
        Map.has_key?(effect, "Confusion") && effect["Confusion"] >= :rand.uniform(100)
      end)
      |> confused(monster, room)
    end

    def confused(nil, %Monster{}, %Room{}), do: false

    def confused(%{"ConfusionMessage" => message} = effect, %Monster{} = monster, %Room{} = room) do
      Mobile.send_scroll(monster, "<p>#{message}</p>")

      if effect["ConfusionSpectatorMessage"],
        do:
          Room.send_scroll(
            room,
            "<p>#{Text.interpolate(effect["ConfusionSpectatorMessage"], %{"user" => monster})}</p>",
            [monster]
          )

      true
    end

    def confused(%{}, %Monster{} = monster, %Room{} = room) do
      send_scroll(monster, "<p><span class='cyan'>You fumble in confusion!</span></p>")

      Room.send_scroll(
        room,
        "<p><span class='cyan'>#{
          Text.interpolate("{{user}} fumbles in confusion!</span></p>", %{"user" => monster})
        }</span></p>",
        [monster]
      )

      true
    end

    def cpr(%Monster{} = monster) do
      time =
        min(Mobile.round_length_in_ms(monster), TimerManager.time_remaining(monster, :heartbeat))

      TimerManager.send_after(monster, {:heartbeat, time, {:heartbeat, monster.ref}})
    end

    def crits_at_level(monster, level) do
      intellect = attribute_at_level(monster, :intellect, level)
      charm = attribute_at_level(monster, :charm, level)

      base = div(intellect * 3 + charm, 6) + level * 2

      trunc(base / (250 + base) * 100) + ability_value(monster, "Crits")
    end

    def description(monster, _observer) do
      monster.description
    end

    def detected?(_monster, sneaker, _room) do
      :rand.uniform(100) >= Mobile.stealth_at_level(sneaker, sneaker.level)
    end

    def die(monster, room) do
      room =
        Enum.reduce(room.mobiles, room, fn
          {ref, %Character{} = character}, updated_room ->
            updated_room =
              Room.update_mobile(updated_room, ref, fn character ->
                message =
                  monster.death_message
                  |> Text.interpolate(%{"name" => monster.name})
                  |> Text.capitalize_first()

                Mobile.send_scroll(character, "<p>#{message}</p>")

                character
                |> Character.add_experience(monster.experience)
                |> Character.add_currency_from_monster(monster)
                |> KillCount.increment(monster)
              end)
              |> Monster.drop_loot_for_character(monster, character)

            Room.update_moblist(updated_room)
            updated_room

          _, updated_room ->
            updated_room
        end)

      ApathyDrive.Repo.delete!(%RoomMonster{id: monster.room_monster_id})

      if monster.regen_time_in_hours do
        spawn_at =
          DateTime.utc_now()
          |> DateTime.to_unix()
          |> Kernel.+(monster.regen_time_in_hours * 60 * 60)

        %Monster{id: monster.id}
        |> Ecto.Changeset.change(next_spawn_at: spawn_at)
        |> Repo.update!()
      end

      room = put_in(room.mobiles, Map.delete(room.mobiles, monster.ref))

      Room.update_moblist(room)
      room
    end

    def dodge_at_level(monster, level, _room) do
      agi = attribute_at_level(monster, :agility, level)
      cha = attribute_at_level(monster, :charm, level)
      base = agi + cha / 10
      trunc(base + ability_value(monster, "Dodge"))
    end

    def block_at_level(monster, _level) do
      trunc(Mobile.ability_value(monster, "Block"))
    end

    def parry_at_level(monster, _level) do
      Mobile.ability_value(monster, "Parry")
    end

    def enough_mana_for_ability?(monster, %Ability{mana: cost}) do
      mana = Mobile.max_mana_at_level(monster, monster.level)

      monster.mana >= cost / mana
    end

    def enter_message(%Monster{name: name}) do
      "<p><span class='yellow'>#{name}</span><span class='dark-green'> walks in from {{direction}}.</span></p>"
    end

    def exit_message(%Monster{name: name}) do
      "<p><span class='yellow'>#{name}</span><span class='dark-green'> walks off {{direction}}.</span></p>"
    end

    def has_ability?(%Monster{} = monster, ability_name) do
      monster.effects
      |> Map.values()
      |> Enum.map(&Map.keys/1)
      |> List.flatten()
      |> Enum.member?(ability_name)
    end

    def exhausted(_monster), do: false

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

    def hp_description(%Monster{hp: hp}) when hp >= 1.0, do: "unwounded"
    def hp_description(%Monster{hp: hp}) when hp >= 0.9, do: "slightly wounded"
    def hp_description(%Monster{hp: hp}) when hp >= 0.6, do: "moderately wounded"
    def hp_description(%Monster{hp: hp}) when hp >= 0.4, do: "heavily wounded"
    def hp_description(%Monster{hp: hp}) when hp >= 0.2, do: "severely wounded"
    def hp_description(%Monster{hp: hp}) when hp >= 0.1, do: "critically wounded"
    def hp_description(%Monster{hp: _hp}), do: "very critically wounded"

    def magical_damage_at_level(monster, level) do
      attribute = attribute_at_level(monster, :intellect, level) - 50

      attribute + ability_value(monster, "ModifyDamage") +
        ability_value(monster, "ModifyMagicalDamage")
    end

    def magical_resistance_at_level(monster, level) do
      willpower = attribute_at_level(monster, :willpower, level)

      max(willpower - 50 + ability_value(monster, "MagicalResist"), 0)
    end

    def max_hp_at_level(%Monster{} = monster, level) do
      health = attribute_at_level(monster, :health, level)

      base = monster.base_hp
      bonus = (health - 50) * level / 16

      trunc(base + bonus + ability_value(monster, "MaxHP"))
    end

    def max_mana_at_level(monster, level) do
      mana_per_level = ability_value(monster, "ManaPerLevel")

      mana_per_level * (level - 1) + 6
    end

    def party_refs(monster, room) do
      Party.refs(room, monster)
    end

    def perception_at_level(monster, level, _room) do
      int = attribute_at_level(monster, :intellect, level)
      cha = attribute_at_level(monster, :charm, level)
      int = int + cha / 10
      modifier = ability_value(monster, "Perception")
      trunc(int * (1 + modifier / 100))
    end

    def physical_damage_at_level(monster, level) do
      attribute = attribute_at_level(monster, :strength, level) - 50

      attribute + ability_value(monster, "ModifyDamage") +
        ability_value(monster, "ModifyMagicalDamage")
    end

    def physical_resistance_at_level(monster, level) do
      strength = attribute_at_level(monster, :strength, level)

      ac = ability_value(monster, "AC")

      max(strength - 50 + ac, 0)
    end

    def power_at_level(%Monster{} = monster, level) do
      [:strength, :agility, :intellect, :willpower, :health, :charm]
      |> Enum.reduce(0, &(&2 + Mobile.attribute_at_level(monster, &1, level)))
    end

    def heartbeat(%Monster{} = monster, %Room{} = room) do
      room =
        Room.update_mobile(room, monster.ref, fn monster ->
          monster
          |> Regeneration.regenerate(room)
          |> RoomServer.execute_casting_ability(room)
        end)
        |> ApathyDrive.Aggression.react(monster.ref)
        |> AI.think(monster.ref)

      if monster = room.mobiles[monster.ref] do
        max_hp = Mobile.max_hp_at_level(monster, monster.level)
        hp = trunc(max_hp * monster.hp)

        if hp < 1 do
          Mobile.die(monster, room)
        else
          room
        end
      else
        room
      end
    end

    def hp_regen_per_round(%Monster{} = monster) do
      round_length = Mobile.round_length_in_ms(monster)

      base_hp_regen =
        (monster.hp_regen +
           (monster.level + 30) * attribute_at_level(monster, :health, monster.level) / 500.0) *
          round_length / 30_000

      modified_hp_regen = base_hp_regen * (1 + ability_value(monster, "HPRegen") / 100)

      max_hp = max_hp_at_level(monster, monster.level)

      modified_hp_regen / max_hp
    end

    def mana_regen_per_round(%Monster{} = monster) do
      round_length = Mobile.round_length_in_ms(monster)

      max_mana = max_mana_at_level(monster, monster.level)

      base_mana_regen =
        (monster.level + 20) * attribute_at_level(monster, :willpower, monster.level) *
          (div(ability_value(monster, "ManaPerLevel"), 2) + 2) / 1650.0 * round_length / 30_000

      modified_mana_regen = base_mana_regen * (1 + ability_value(monster, "ManaRegen") / 100)

      if max_mana > 0 do
        modified_mana_regen / max_mana
      else
        0
      end
    end

    def round_length_in_ms(monster) do
      speed = ability_value(monster, "Speed")

      modifier =
        if speed == 0 do
          1
        else
          speed / 100
        end

      trunc(modifier * Application.get_env(:apathy_drive, :round_length_in_ms))
    end

    def send_scroll(%Monster{} = monster, _html) do
      monster
    end

    def set_room_id(%Monster{} = monster, room_id) do
      %RoomMonster{id: monster.room_monster_id}
      |> Ecto.Changeset.change(%{room_id: room_id})
      |> Repo.update!()

      monster
      |> Map.put(:last_room_id, monster.room_id)
      |> Map.put(:room_id, room_id)
    end

    def shift_hp(monster, percentage) do
      update_in(monster.hp, &min(1.0, &1 + percentage))
    end

    def silenced(%Monster{effects: effects} = monster, %Room{} = room) do
      effects
      |> Map.values()
      |> Enum.find(fn effect ->
        Map.has_key?(effect, "Silence")
      end)
      |> silenced(monster, room)
    end

    def silenced(nil, %Monster{}, %Room{}), do: false

    def silenced(%{}, %Monster{} = monster, %Room{}) do
      Mobile.send_scroll(monster, "<p><span class='cyan'>You are silenced!</span></p>")
      true
    end

    def spellcasting_at_level(monster, level, ability) do
      sc =
        ability.attributes
        |> Map.keys()
        |> case do
          [:intellect] ->
            intellect = Mobile.attribute_at_level(monster, :intellect, level)
            willpower = Mobile.attribute_at_level(monster, :willpower, level)

            trunc((intellect * 3 + willpower * 3) / 6 + level * 2)

          [:willpower] ->
            intellect = Mobile.attribute_at_level(monster, :intellect, level)
            willpower = Mobile.attribute_at_level(monster, :willpower, level)

            trunc((willpower * 3 + intellect * 3) / 6 + level * 2)

          [:agility] ->
            agility = Mobile.attribute_at_level(monster, :agility, level)
            willpower = Mobile.attribute_at_level(monster, :willpower, level)

            trunc((agility * 3 + willpower * 3) / 6 + level * 2)

          [:intellect, :willpower] ->
            intellect = Mobile.attribute_at_level(monster, :intellect, level)
            willpower = Mobile.attribute_at_level(monster, :willpower, level)

            trunc((willpower + intellect) / 3 + level * 2)

          [:charm] ->
            charm = Mobile.attribute_at_level(monster, :charm, level)
            willpower = Mobile.attribute_at_level(monster, :willpower, level)

            trunc((charm * 3 + willpower * 3) / 6 + level * 2)
        end

      sc + ability_value(monster, "Spellcasting")
    end

    def stealth_at_level(monster, level) do
      agi = attribute_at_level(monster, :agility, level)
      cha = attribute_at_level(monster, :charm, level)
      agi = agi + cha / 10
      modifier = ability_value(monster, "Stealth")
      trunc(agi * (modifier / 100))
    end

    def subtract_mana(monster, %{mana: cost} = _ability) do
      percentage = cost / Mobile.max_mana_at_level(monster, monster.level)
      update_in(monster.mana, &max(0, &1 - percentage))
    end

    def subtract_energy(monster, ability) do
      initial_energy = monster.energy
      monster = update_in(monster.energy, &max(0, &1 - ability.energy))

      if initial_energy == monster.max_energy do
        Regeneration.schedule_next_tick(monster)
      else
        monster
      end
    end

    def target_level(%Monster{level: monster_level}, %Monster{level: _target_level}),
      do: monster_level

    def target_level(%Monster{level: monster_level}, %{level: target_level}),
      do: max(target_level, monster_level)

    def tracking_at_level(monster, level, room) do
      perception = perception_at_level(monster, level, room)
      modifier = ability_value(monster, "Tracking")
      perception * (modifier / 100)
    end

    def update_prompt(%Monster{} = monster) do
      monster
    end
  end
end
