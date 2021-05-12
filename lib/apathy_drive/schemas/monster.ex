defmodule ApathyDrive.Monster do
  use Ecto.Schema
  use ApathyDriveWeb, :model

  alias ApathyDrive.{
    Ability,
    Affix,
    AffixSkill,
    Aggression,
    AI,
    ChannelHistory,
    Character,
    Currency,
    Regeneration,
    Item,
    ItemInstance,
    KillCount,
    LootPity,
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
    TimerManager,
    Trait
  }

  require Logger

  @behaviour Access
  defdelegate get_and_update(container, key, fun), to: Map
  defdelegate fetch(container, key), to: Map
  defdelegate get(container, key, default), to: Map
  defdelegate pop(container, key), to: Map

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
    field(:npc, :boolean)

    field(:hate, :map, virtual: true, default: %{})
    field(:base_name, :string, virtual: true)
    field(:bust_cache, :boolean, virtual: true, default: false)
    field(:last_auto_attack_at, :any, virtual: true)
    field(:owner_id, :integer, virtual: true)
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
    field(:auto_roam, :boolean, virtual: true, default: true)
    field(:drops, :any, virtual: true, default: [])
    field(:sneaking, :boolean, virtual: true, default: false)
    field(:delete_at, :any, virtual: true)
    field(:lore, :any, virtual: true)
    field(:follow, :boolean, virtual: true)

    timestamps()

    belongs_to(:death_ability, ApathyDrive.Ability)

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
    monster.hate
    |> Map.keys()
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
         {:follow?, true} <-
           {:follow?, :rand.uniform(100) < monster.chance_to_follow and !monster.follow},
         {:exit, %{} = room_exit} <- {:exit, chase_exit(monster, room, destination)} do
      case ApathyDrive.Commands.Move.execute(room, monster, room_exit) do
        %Room{} = room ->
          room

        {:error, :too_tired, room} ->
          room
      end
    else
      _ ->
        if monster.owner_id == character.id and monster.follow do
          room_exit =
            room.exits
            |> Enum.find(&(&1["destination"] == destination))

          case room_exit do
            %{"kind" => "Command"} ->
              ApathyDrive.Commands.Move.execute(
                room,
                monster,
                Map.put(room_exit, "kind", "Action")
              )

            _ ->
              ApathyDrive.Commands.Move.execute(room, monster, room_exit)
          end
        else
          room
        end
    end
  end

  def from_room_monster(room_monster, force \\ false)

  def from_room_monster(
        %RoomMonster{id: nil, room_id: room_id, monster_id: monster_id} = rm,
        force
      ) do
    now =
      DateTime.utc_now()
      |> DateTime.to_unix()

    monster =
      Repo.get(Monster, monster_id)
      |> Map.put(:room_id, room_id)
      |> Map.put(:level, rm.level)
      |> Map.put(:spawned_at, rm.spawned_at)
      |> Map.put(:zone_spawned_at, rm.zone_spawned_at)
      |> Map.put(:delete_at, rm.delete_at)
      |> Map.put(:owner_id, rm.owner_id)
      |> Map.put(:lore, ApathyDrive.ElementalLores.lore(rm.lore))

    if force or (!MonsterSpawning.limit_reached?(monster) and spawnable?(monster, now)) do
      ref = :crypto.hash(:md5, inspect(make_ref())) |> Base.encode16()

      monster
      |> Map.put(:ref, ref)
      |> generate_monster_attributes()
      |> load_abilities()
      |> load_traits()
      |> load_drops()
      |> TimerManager.send_after({:heartbeat, 100, {:heartbeat, ref}})
    end
  end

  def from_room_monster(%RoomMonster{id: id, monster_id: monster_id} = rm, _force) do
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
        :room_spawned_at,
        :delete_at,
        :owner_id
      ])

    ref = :crypto.hash(:md5, inspect(make_ref())) |> Base.encode16()

    monster
    |> Map.merge(attributes)
    |> Map.put(:lore, ApathyDrive.ElementalLores.lore([rm.lore]))
    |> Map.put(:room_monster_id, id)
    |> Map.put(:ref, ref)
    |> Map.put(:level, rm.level)
    |> Map.put(:base_name, monster.name)
    |> load_abilities()
    |> load_traits()
    |> load_drops()
    |> TimerManager.send_after({:heartbeat, 100, {:heartbeat, ref}})
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
    name =
      if monster.lore do
        monster.lore.name <> " " <> monster.name
      else
        monster.name
      end

    room_monster = %RoomMonster{
      level: level,
      room_id: monster.room_id,
      monster_id: monster.id,
      spawned_at: monster.spawned_at,
      zone_spawned_at: monster.zone_spawned_at,
      name: name_with_adjective(name, monster.adjectives),
      delete_at: monster.delete_at,
      owner_id: monster.owner_id
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
    |> Map.put(:base_name, monster.name)
  end

  def generate_monster_attributes(%Monster{} = monster), do: monster

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

  def auto_attack_target(%Monster{hate: hate}, room) do
    here =
      room.mobiles
      |> Map.keys()

    hate
    |> Enum.reject(fn {ref, _enmity} ->
      !(ref in here)
    end)
    |> Enum.sort_by(fn {_ref, enmity} -> enmity end, &>=/2)
    |> List.first()
    |> case do
      {ref, _enmity} ->
        ref

      _ ->
        nil
    end
  end

  def drop_loot_for_character(
        %Room{} = room,
        %Monster{} = monster,
        %Character{} = character
      ) do
    room =
      Enum.reduce(monster.drops, room, fn %{chance: chance, item_id: item_id, id: drop_id},
                                          room ->
        pity = LootPity.pity_for_character(character, drop_id)

        if :rand.uniform(100) <= chance + pity do
          Logger.info("Dropping item##{item_id} for #{character.name} in Room##{room.id}")

          item =
            %ItemInstance{
              item_id: item_id,
              room_id: room.id,
              character_id: nil,
              equipped: false,
              hidden: false,
              level: max(1, character.level),
              delete_at: Timex.shift(DateTime.utc_now(), hours: 1)
            }
            |> Repo.insert!()
            |> Repo.preload(:item)
            |> Item.from_assoc()

          Mobile.send_scroll(
            character,
            "<p>A #{Item.colored_name(item, character: character)} drops to the floor.</p>"
          )

          LootPity.reset_pity(character, drop_id)
        else
          LootPity.increase_pity(character, drop_id)
        end

        room
      end)

    drop_random_loot_for_character(room, monster, character)

    room
    |> Room.load_items()
  end

  def drop_random_loot_for_character(
        %Room{} = room,
        %Monster{} = monster,
        %Character{} = character,
        quality \\ nil
      ) do
    level = (div(monster.level, 3) + 1) * 3

    items = Item.of_quality_level(level)

    picks = if monster.game_limit == 1, do: 5, else: 1

    Enum.each(1..picks, fn _pick ->
      roll = :rand.uniform(100)

      chance = if monster.game_limit == 1, do: 80, else: 40

      if Enum.any?(items) and (quality || roll <= chance) do
        item =
          items
          |> Enum.random()
          |> Item.load_item_types()

        Logger.info("Dropping item##{item.id} for #{character.name} in Room##{room.id}")

        quality = quality || determine_item_quality(character, monster, item)

        ac = ac_for_item(item, quality)

        item_instance =
          %ItemInstance{
            item_id: item.id,
            room_id: room.id,
            character_id: nil,
            equipped: false,
            hidden: false,
            ac: ac,
            name: item.name,
            quality: quality,
            level: max(1, monster.level),
            delete_at: Item.delete_at(item.quality)
          }
          |> Repo.insert!()
          |> Repo.preload(:item)
          |> update_in([Access.key!(:item)], &Item.load_item_types/1)

        affix_level = affix_level(item.quality_level, monster.level, 0)

        {prefixes, suffixes} = item_affixes(item_instance, affix_level)

        prefixes = Enum.reject(prefixes, &is_nil/1)
        suffixes = Enum.reject(suffixes, &is_nil/1)

        name = item_name(item_instance, prefixes, suffixes)

        item_instance =
          item_instance
          |> Ecto.Changeset.change(%{name: name})
          |> Repo.update!()

        item =
          item_instance
          |> Item.from_assoc()

        Mobile.send_scroll(
          character,
          "<p>A #{Item.colored_name(item, character: character)} drops to the floor.</p>"
        )
      else
        chance = if monster.game_limit == 1, do: 60, else: 30
        # drop jewelry, runes, gems etc
        if :rand.uniform(100) < chance do
          item =
            Item.random_accessory()
            |> Item.load_item_types()

          Logger.info("Dropping item##{item.id} for #{character.name} in Room##{room.id}")

          quality = quality || determine_item_quality(character, monster, item)

          item_instance =
            %ItemInstance{
              item_id: item.id,
              room_id: room.id,
              character_id: nil,
              equipped: false,
              hidden: false,
              ac: 0,
              name: item.name,
              quality: quality,
              level: max(1, monster.level),
              delete_at: Item.delete_at(item.quality)
            }
            |> Repo.insert!()
            |> Repo.preload(:item)
            |> update_in([Access.key!(:item)], &Item.load_item_types/1)

          affix_level = affix_level(item.quality_level, monster.level, character.level)

          {prefixes, suffixes} = item_affixes(item_instance, affix_level)

          prefixes = Enum.reject(prefixes, &is_nil/1)
          suffixes = Enum.reject(suffixes, &is_nil/1)

          name = item_name(item_instance, prefixes, suffixes)

          item_instance =
            item_instance
            |> Ecto.Changeset.change(%{name: name})
            |> Repo.update!()

          item =
            item_instance
            |> Item.from_assoc()

          Mobile.send_scroll(
            character,
            "<p>A #{Item.colored_name(item, character: character)} drops to the floor.</p>"
          )
        end
      end
    end)
  end

  def ac_for_item(%{min_ac: nil, max_ac: nil}, _quality), do: nil

  def ac_for_item(item, "low") do
    ac = Enum.random(item.min_ac..item.max_ac)
    trunc(ac * 0.75)
  end

  def ac_for_item(item, _quality) do
    Enum.random(item.min_ac..item.max_ac)
  end

  def affix_level(quality_level, monster_level, magic_level) do
    monster_level = min(monster_level, 99)

    monster_level = max(quality_level, monster_level)

    affix_level =
      if magic_level > 0 do
        monster_level + magic_level
      else
        if monster_level < 99 - quality_level / 2 do
          monster_level - div(quality_level, 2)
        else
          2 * monster_level - 99
        end
      end

    min(affix_level, 99)
  end

  def item_affixes(
        %ItemInstance{quality: "superior", item: %Item{type: "Armour"}} = item_instance,
        _affix_level
      ) do
    affix =
      Affix
      |> Repo.get_by(name: "superior armor")
      |> Repo.preload(affixes_traits: [:trait])

    affix.affixes_traits
    |> Enum.each(fn at ->
      val = affix_value(at.value, at.trait.merge_by)

      %ApathyDrive.ItemInstanceAffixTrait{
        affix_trait_id: at.id,
        item_instance_id: item_instance.id,
        value: val,
        description: affix_description(at.trait.name, at.description, val)
      }
      |> Repo.insert!()
    end)

    {[], []}
  end

  def item_affixes(%ItemInstance{quality: "magic"} = item_instance, affix_level) do
    case :rand.uniform(4) do
      4 ->
        {[generate_prefix(item_instance, affix_level)],
         [generate_suffix(item_instance, affix_level)]}

      3 ->
        {[generate_prefix(item_instance, affix_level)], []}

      _ ->
        {[], [generate_suffix(item_instance, affix_level)]}
    end
  end

  def item_affixes(%ItemInstance{quality: "rare"} = item_instance, affix_level) do
    affix_count = Enum.random(3..6)

    1..affix_count
    |> Enum.reduce({[], []}, fn _n, {prefixes, suffixes} ->
      cond do
        length(prefixes) >= 3 ->
          {prefixes, [generate_suffix(item_instance, affix_level) | suffixes]}

        length(suffixes) >= 3 ->
          {[generate_prefix(item_instance, affix_level) | prefixes], suffixes}

        :else ->
          case :rand.uniform(2) do
            1 ->
              {[generate_prefix(item_instance, affix_level) | prefixes], suffixes}

            2 ->
              {prefixes, [generate_suffix(item_instance, affix_level) | suffixes]}
          end
      end
    end)
  end

  def item_affixes(%ItemInstance{}, _affix_level) do
    {[], []}
  end

  def generate_prefix(item_instance, affix_level) do
    prefix =
      affix_level
      |> Affix.prefix_for_level(item_instance)

    if prefix do
      prefix =
        prefix
        |> Repo.preload(affixes_traits: [:trait], affix_skills: [:skill])

      if prefix.affixes_traits == [] and prefix.affix_skills == [] do
        IO.puts("no traits or skills, trying again")
        generate_prefix(item_instance, affix_level)
      else
        prefix.affixes_traits
        |> Enum.each(fn at ->
          val = affix_value(at.value, at.trait.merge_by)

          %ApathyDrive.ItemInstanceAffixTrait{
            affix_trait_id: at.id,
            item_instance_id: item_instance.id,
            value: val,
            description: affix_description(at.trait.name, at.description, val)
          }
          |> Repo.insert!()
        end)

        prefix.affix_skills
        |> Enum.each(fn
          %AffixSkill{
            value: %{
              "kind" => "Charges",
              "base_charges" => base_charges,
              "max_level" => max_skill_level
            }
          } = as ->
            required_skill_level = as.skill.required_level
            item_level = item_instance.level
            skill_level = charged_skill_level(required_skill_level, max_skill_level, item_level)
            charges = charged_skill_charges(base_charges, skill_level)

            %ApathyDrive.ItemInstanceAffixSkill{
              affix_skill_id: as.id,
              item_instance_id: item_instance.id,
              value: %{"charges" => charges, "level" => skill_level},
              description: as.description
            }
            |> Repo.insert!()

          %AffixSkill{value: value} = as ->
            %ApathyDrive.ItemInstanceAffixSkill{
              affix_skill_id: as.id,
              item_instance_id: item_instance.id,
              value: value,
              description: as.description
            }
            |> Repo.insert!()
        end)

        prefix.name
      end
    end
  end

  def generate_suffix(item_instance, affix_level) do
    suffix =
      affix_level
      |> Affix.suffix_for_level(item_instance)

    if suffix do
      suffix =
        suffix
        |> Repo.preload(affixes_traits: [:trait], affix_skills: [:skill])

      if suffix.affixes_traits == [] and suffix.affix_skills == [] do
        IO.puts("no traits or skills, trying again")
        generate_suffix(item_instance, affix_level)
      else
        suffix.affixes_traits
        |> Enum.each(fn at ->
          val = affix_value(at.value, at.trait.merge_by)

          %ApathyDrive.ItemInstanceAffixTrait{
            affix_trait_id: at.id,
            item_instance_id: item_instance.id,
            value: val,
            description: affix_description(at.trait.name, at.description, val)
          }
          |> Repo.insert!()
        end)

        suffix.affix_skills
        |> Enum.each(fn
          %AffixSkill{
            value: %{
              "kind" => "Charges",
              "base_charges" => base_charges,
              "max_level" => max_skill_level
            }
          } = as ->
            required_skill_level = as.skill.required_level
            item_level = item_instance.level
            skill_level = charged_skill_level(required_skill_level, max_skill_level, item_level)
            charges = charged_skill_charges(base_charges, skill_level)

            %ApathyDrive.ItemInstanceAffixSkill{
              affix_skill_id: as.id,
              item_instance_id: item_instance.id,
              value: %{"charges" => charges, "level" => skill_level},
              description: as.description
            }
            |> Repo.insert!()

          %AffixSkill{value: value} = as ->
            %ApathyDrive.ItemInstanceAffixSkill{
              affix_skill_id: as.id,
              item_instance_id: item_instance.id,
              value: value,
              description: as.description
            }
            |> Repo.insert!()
        end)

        suffix.name
      end
    end
  end

  def charged_skill_level(required_skill_level, max_skill_level, item_level) do
    modifier = trunc((99 - required_skill_level) / max_skill_level)
    max(1, trunc((item_level - required_skill_level) / modifier))
  end

  def charged_skill_charges(base_charges, skill_level) do
    trunc((base_charges + base_charges * skill_level) / 8)
  end

  def affix_description("DefensePerLevel", description, _val) do
    description
  end

  def affix_description("HalfFreezeDuration", description, _val) do
    description
  end

  def affix_description(_trait_name, description, [val]) do
    ApathyDrive.Text.interpolate(description, val)
  end

  def affix_description(_trait_name, description, val) when is_integer(val) do
    ApathyDrive.Text.interpolate(description, %{"amount" => val})
  end

  def affix_value(
        %{
          "max" => %{"max" => max_max, "min" => min_max},
          "min" => %{"max" => max_min, "min" => min_min}
        },
        merge_by
      ) do
    affix_value(
      %{"max" => Enum.random(min_max..max_max), "min" => Enum.random(min_min..max_min)},
      merge_by
    )
  end

  def affix_value(val, "list") do
    [val]
  end

  def affix_value(%{"min" => min, "max" => max}, _merge_by) do
    Enum.random(min..max)
  end

  def affix_value(value, _merge_by) do
    value
  end

  def item_name(%ItemInstance{quality: "superior"} = item, _prefixes, _suffixes) do
    "superior #{item.name}"
  end

  def item_name(%ItemInstance{quality: "low"} = item, _prefixes, _suffixes) do
    prefix = Enum.random(["cracked", "damaged", "low quality", "crude"])
    "#{prefix} #{item.name}"
  end

  def item_name(%ItemInstance{quality: quality} = item, prefixes, suffixes)
      when quality in ["magic", "rare"] do
    prefix =
      if Enum.any?(prefixes) do
        Enum.random(prefixes)
      else
        ""
      end

    suffix =
      if Enum.any?(suffixes) do
        Enum.random(suffixes)
      else
        ""
      end

    "#{prefix} #{item.name} #{suffix}"
    |> String.trim()
    |> String.downcase()
  end

  def item_name(%ItemInstance{quality: _} = item, _prefixes, _suffixes) do
    item.name
  end

  def determine_item_quality(character, monster, item) do
    magic_find =
      Mobile.ability_value(character, "MagicFind") +
        Mobile.attribute_at_level(character, :charm, character.level)

    magic_find = if monster.game_limit == 1, do: 400 + magic_find * 2, else: magic_find

    cond do
      unique?(monster.level, item.quality_level, magic_find) ->
        IO.puts("dropped unique!")
        "rare"

      set?(monster.level, item.quality_level, magic_find) ->
        IO.puts("dropped set!")
        "rare"

      rare?(monster.level, item.quality_level, magic_find) ->
        IO.puts("dropped rare!")
        "rare"

      magic?(monster.level, item, magic_find) ->
        IO.puts("dropped magic!")
        "magic"

      high?(monster.level, item.quality_level, magic_find) ->
        IO.puts("dropped superior!")
        "superior"

      normal?(monster.level, item.quality_level, magic_find) ->
        IO.puts("dropped normal!")
        "normal"

      :else ->
        "low"
    end
  end

  def unique?(monster_level, quality_level, magic_find) do
    chance = (400 - (monster_level - quality_level) / 1) * 128

    chance = max(6400, trunc(chance * 100 / (100 + magic_find))) |> max(1)

    IO.puts("unique chance: #{128 / chance * 100}%")
    :rand.uniform(chance) < 128
  end

  def set?(monster_level, quality_level, magic_find) do
    chance = (160 - (monster_level - quality_level) / 2) * 128

    chance = max(5600, trunc(chance * 100 / (100 + magic_find))) |> max(1)
    IO.puts("set chance: #{128 / chance * 100}%")
    :rand.uniform(chance) < 128
  end

  def rare?(monster_level, quality_level, magic_find) do
    chance = (100 - (monster_level - quality_level) / 2) * 128

    chance = max(3200, trunc(chance * 100 / (100 + magic_find))) |> max(1)
    IO.puts("rare chance: #{128 / chance * 100}%")
    :rand.uniform(chance) < 128
  end

  def magic?(monster_level, %Item{quality_level: quality_level} = item, magic_find) do
    item_types = Enum.map(item.item_types, & &1.name)

    if "Ring" in item_types or "Amulet" in item_types do
      true
    else
      chance = (34 - (monster_level - quality_level) / 3) * 128

      chance = max(192, trunc(chance * 100 / (100 + magic_find))) |> max(1)
      IO.puts("magic chance: #{128 / chance * 100}%")
      :rand.uniform(chance) < 128
    end
  end

  def high?(monster_level, quality_level, magic_find) do
    chance = (12 - (monster_level - quality_level) / 8) * 128

    chance = trunc(chance * 100 / (100 + magic_find)) |> max(1)
    IO.puts("superior chance: #{128 / chance * 100}%")
    :rand.uniform(chance) < 128
  end

  def normal?(monster_level, quality_level, magic_find) do
    chance = (2 - (monster_level - quality_level) / 2) * 128
    IO.puts("monster_level: #{monster_level}, quality_level: #{quality_level}")
    IO.puts("chance: #{chance}")

    chance = trunc(chance * 100 / (100 + magic_find)) |> max(1)
    IO.puts("chance: #{chance}")
    IO.puts("normal chance: #{128 / chance * 100}%")
    :rand.uniform(chance) < 128
  end

  defimpl ApathyDrive.Mobile, for: Monster do
    def ability_value(monster, ability) do
      Trait.get_cached(monster, ability)
    end

    def accuracy_at_level(monster, _level, _room) do
      attack_rating(monster)
    end

    def attribute_at_level(%Monster{} = monster, attribute, level) do
      Map.get(monster, attribute) + level - 1 +
        ability_value(monster, attribute |> to_string |> String.capitalize())
    end

    def attack_ability(monster) do
      monster.abilities
      |> Map.values()
      |> Enum.filter(&(&1.kind == "auto attack" or !is_nil(&1.chance)))
      |> case do
        [] ->
          nil

        attacks ->
          attacks =
            attacks
            |> Enum.sort_by(& &1.chance)

          attack =
            attacks
            |> Enum.find(fn attack ->
              rand = :rand.uniform(100)

              attack.chance > rand or attack == List.last(attacks)
            end)

          auto_attack? = attack.kind == "auto attack"

          if auto_attack? do
            damage = Mobile.ability_value(monster, "WeaponDamage")
            update_in(attack.traits["Damage"], &(damage ++ &1))
          else
            attack
          end
          |> Map.put(:kind, "attack")
      end
    end

    def attack_rating(monster) do
      (8 + (monster.level - 1) * 0.3245) * monster.level
    end

    def auto_attack_target(%Monster{} = monster, room) do
      Monster.auto_attack_target(monster, room)
    end

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

    def die?(monster) do
      max_hp = Mobile.max_hp_at_level(monster, monster.level)

      max_hp <= 0 or monster.hp <= 0
    end

    def die(monster, room) do
      pets_and_players =
        room.mobiles
        |> Enum.reduce([], fn
          {_ref, %Character{} = character}, pets_and_players ->
            AI.pets_and_party(room, character) ++ pets_and_players

          _mobile, pets_and_players ->
            pets_and_players
        end)
        |> Enum.uniq()
        |> length()

      message =
        monster.death_message
        |> Text.interpolate(%{"name" => monster.name})
        |> Text.capitalize_first()

      room =
        Enum.reduce(room.mobiles, room, fn
          {ref, %Character{} = character}, updated_room ->
            updated_room =
              Room.update_mobile(updated_room, ref, fn _updated_room, character ->
                Mobile.send_scroll(character, "<p>#{message}</p>")

                exp = max(1, div(monster.experience, pets_and_players))

                character
                |> Character.add_experience(exp)
                |> Character.add_currency_from_monster(monster)
                |> Character.execute_per_kill_traits()
                |> KillCount.increment(monster, exp)
              end)

            updated_room =
              Monster.drop_loot_for_character(
                updated_room,
                monster,
                room.mobiles[character.ref]
              )

            Room.update_moblist(updated_room)
            updated_room

          _, updated_room ->
            updated_room
        end)

      if monster.regen_time_in_hours do
        spawn_at =
          DateTime.utc_now()
          |> DateTime.to_unix()
          |> Kernel.+(monster.regen_time_in_hours * 60 * 60)

        %Monster{id: monster.id}
        |> Ecto.Changeset.change(next_spawn_at: spawn_at)
        |> Repo.update!()
      end

      if monster.game_limit && monster.regen_time_in_hours do
        message = "<p>[<span class='yellow'>announce</span> : Apotheosis] #{message}</p>"

        Repo.insert!(%ChannelHistory{
          character_name: "Apotheosis",
          channel_name: "announce",
          message: message
        })

        ApathyDriveWeb.Endpoint.broadcast!("chat:gossip", "chat-sidebar", %{
          html: message,
          chat_tab: "announce"
        })
      end

      room =
        if monster.death_ability_id do
          ability =
            monster.death_ability_id
            |> Ability.find()
            |> Map.put(:energy, 0)

          Ability.execute(room, monster.ref, ability, [monster.ref])
        else
          room
        end

      ApathyDrive.Repo.delete!(%RoomMonster{id: monster.room_monster_id})

      room = put_in(room.mobiles, Map.delete(room.mobiles, monster.ref))

      Room.update_moblist(room)
      room
    end

    def dodge_at_level(monster, _level, _room) do
      trunc(defense_rating(monster) + ability_value(monster, "Dodge"))
    end

    def enough_mana_for_ability?(monster, %Ability{mana: cost}) do
      mana = Mobile.max_mana_at_level(monster, monster.level)

      monster.mana >= cost / mana
    end

    def evil_points(monster, %Character{} = attacker) do
      cond do
        Ability.retaliate?(monster, attacker) ->
          # attacker has already received evil points for attacking monster
          0

        Ability.retaliate?(attacker, monster) ->
          # monster has attacked the character, it's not evil to fight back
          0

        monster.alignment == "good" ->
          40

        :else ->
          0
      end
    end

    # only characters can receive evil points
    def evil_points(_monster, %{} = _attacker), do: 0

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

    def exhausted(_monster, _req \\ nil), do: false

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

    def hp_description(%Monster{hp: hp}) when hp >= 1.0, do: "unwounded"
    def hp_description(%Monster{hp: hp}) when hp >= 0.9, do: "slightly wounded"
    def hp_description(%Monster{hp: hp}) when hp >= 0.6, do: "moderately wounded"
    def hp_description(%Monster{hp: hp}) when hp >= 0.4, do: "heavily wounded"
    def hp_description(%Monster{hp: hp}) when hp >= 0.2, do: "severely wounded"
    def hp_description(%Monster{hp: hp}) when hp >= 0.1, do: "critically wounded"
    def hp_description(%Monster{hp: _hp}), do: "very critically wounded"

    def magical_resistance_at_level(monster, _level) do
      defense_rating(monster)
    end

    def max_hp_at_level(%Monster{} = monster, level) do
      base = 7 + (2.5 + level * 0.585) * level

      max_hp_percent = ability_value(monster, "MaxHP%")

      modifier = if max_hp_percent > 0, do: max_hp_percent, else: 1.0

      trunc((base + ability_value(monster, "MaxHP")) * modifier)
    end

    def max_mana_at_level(monster, level) do
      mana_per_level = ability_value(monster, "ManaPerLevel")

      mana_per_level = if mana_per_level == 0, do: 4, else: 0

      bonus = ability_value(monster, "MaxMana")

      mana_per_level * (level - 1) + 6 + bonus
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

    def defense_rating(monster) do
      (6 + (monster.level - 1) * 0.105) * monster.level
    end

    def physical_resistance_at_level(monster, _level) do
      defense_rating(monster)
    end

    def power_at_level(%Monster{} = monster, level) do
      [:strength, :agility, :intellect, :willpower, :health, :charm]
      |> Enum.reduce(0, &(&2 + Mobile.attribute_at_level(monster, &1, level)))
    end

    def heartbeat(%Monster{} = monster, %Room{} = room) do
      room =
        room
        |> Room.update_mobile(monster.ref, fn room, monster ->
          cond do
            monster.delete_at && :lt == DateTime.compare(monster.delete_at, DateTime.utc_now()) ->
              Room.send_scroll(
                room,
                "<p><span class='dark-yellow'>#{monster.name} winks out of existence.</span></p>"
              )

              ApathyDrive.Repo.delete!(%RoomMonster{id: monster.room_monster_id})

              room = put_in(room.mobiles, Map.delete(room.mobiles, monster.ref))

              Room.update_moblist(room)
              room

            Mobile.die?(monster) ->
              Mobile.die(monster, room)

            :else ->
              monster
          end
        end)
        |> Ability.unbalance(monster.ref)
        |> Room.update_mobile(monster.ref, fn room, monster ->
          if monster.bust_cache, do: Trait.bust_cache(monster)

          monster
          |> Map.put(:bust_cache, false)
          |> Regeneration.regenerate(room)
          |> TimerManager.send_after(
            {:heartbeat, ApathyDrive.Regeneration.tick_time(monster), {:heartbeat, monster.ref}}
          )
          |> RoomServer.execute_casting_ability(room)
        end)

      if :rand.uniform(100) > 75 do
        ApathyDrive.Aggression.react(room, monster.ref)
      else
        room
      end
      |> AI.think(monster.ref)
    end

    def hp_regen_per_30(%Monster{} = monster) do
      monster.hp_regen * (1 + ability_value(monster, "HPRegen") / 100)
    end

    def mana_regen_per_30(%Monster{} = monster) do
      max_mana = max_mana_at_level(monster, monster.level)

      base_mana_regen = max_mana * 0.2

      modified_mana_regen = base_mana_regen * (1 + ability_value(monster, "ManaRegen") / 100)

      if max_mana > 0 do
        modified_mana_regen
      else
        0
      end
    end

    def send_scroll(%Monster{} = monster, _html) do
      monster
    end

    def set_room_id(%Monster{} = monster, room_id) do
      %RoomMonster{id: monster.room_monster_id}
      |> Ecto.Changeset.change(%{room_id: room_id})
      |> Repo.update()

      monster
      |> Map.put(:last_room_id, monster.room_id)
      |> Map.put(:room_id, room_id)
    end

    def shift_hp(monster, percentage) do
      update_in(monster.hp, &max(0.0, min(1.0, &1 + percentage)))
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

    def spellcasting_at_level(monster, level, _ability) do
      attribute_value = Room.average([monster.intellect, monster.willpower])

      sc = attribute_value + level * 2

      trunc(sc + ability_value(monster, "Spellcasting"))
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
      update_in(monster.energy, &(&1 - ability.energy))
    end

    def tracking_at_level(monster, level, room) do
      perception = perception_at_level(monster, level, room)
      modifier = ability_value(monster, "Tracking")
      perception * (modifier / 100)
    end

    def update_prompt(%Monster{} = monster, room) do
      Room.update_hp_bar(room, monster.ref)
      Room.update_mana_bar(room, monster.ref)
      monster
    end
  end
end
