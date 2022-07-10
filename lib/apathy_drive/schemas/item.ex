defmodule ApathyDrive.Item do
  use ApathyDriveWeb, :model

  alias ApathyDrive.{
    Ability,
    Character,
    Currency,
    Enchantment,
    Item,
    ItemClass,
    ItemInstance,
    ItemInstanceAffixTrait,
    ItemInstanceAffixSkill,
    ItemType,
    ItemTypeParent,
    ItemRace,
    Match,
    Mobile,
    PubSub,
    Regeneration,
    RoomServer,
    ShopItem,
    Socket,
    Trait
  }

  @types [
    "Armour",
    "Container",
    "Drink",
    "Food",
    "Key",
    "Light",
    "Projectile",
    "Scroll",
    "Shield",
    "Sign",
    "Special",
    "Weapon"
  ]

  @armour_type_modifiers %{
    "platemail armour" => 1,
    "scalemail armour" => 0.875,
    "chainmail armour" => 0.75,
    "leather armour" => 0.625,
    "cloth armour" => 0.5
  }

  require Logger
  require Ecto.Query

  schema "items" do
    field(:name, :string)
    field(:type, :string)
    field(:worn_on, :string)
    field(:weapon_type, :string)
    field(:armour_type, :string)
    field(:game_limit, :integer)
    field(:weight, :integer)
    field(:speed, :integer)
    field(:max_uses, :integer)
    field(:getable, :boolean)
    field(:droppable, :boolean)
    field(:destroy_on_death, :boolean)
    field(:destroy_when_fully_used, :boolean)
    field(:robbable, :boolean)
    field(:cost_value, :integer)
    field(:cost_currency, :string)
    field(:min_damage, :integer)
    field(:max_damage, :integer)
    field(:description, :string)
    field(:hit_verbs, ApathyDrive.JSONB)
    field(:miss_verbs, ApathyDrive.JSONB)
    field(:destruct_message, :string)
    field(:room_destruct_message, :string)
    field(:global_drop_rarity, :string)
    field(:level, :integer)
    field(:quality_level, :integer)
    field(:max_sockets, :integer)
    field(:magic_level, :integer)
    field(:type_id, :integer)
    field(:block_chance, :integer)
    field(:socketable, :boolean)
    field(:stackable, :boolean)

    field(:short_name, :string, virtual: true)
    field(:ac, :any, virtual: true, default: 0)
    field(:sockets, :any, virtual: true, default: [])
    field(:item_type_ids, :any, virtual: true, default: [])
    field(:item_types, :any, virtual: true, default: [])
    field(:affix_traits, :any, virtual: true, default: [])
    field(:affix_skills, :any, virtual: true, default: [])
    field(:quality, :any, virtual: true)
    field(:equipped, :boolean, virtual: true, default: false)
    field(:beacon_room_id, :any, virtual: true)
    field(:owner_id, :any, virtual: true)
    field(:instance_id, :integer, virtual: true)
    field(:delete_at, :utc_datetime_usec, virtual: true)
    field(:effects, :map, virtual: true, default: %{})
    field(:last_effect_key, :integer, virtual: true, default: 0)
    field(:timers, :map, virtual: true, default: %{})
    field(:required_races, :any, virtual: true, default: [])
    field(:required_classes, :any, virtual: true, default: [])
    field(:enchantments, :any, virtual: true, default: [])
    field(:keywords, :any, virtual: true)
    field(:uses, :float, virtual: true)
    field(:hidden, :boolean, virtual: true)
    field(:unfinished, :boolean, virtual: true)

    has_many(:items_instances, ApathyDrive.ItemInstance)
    has_many(:socketable_item_affixes, ApathyDrive.SocketableItemAffix)
  end

  @required_fields ~w()a
  @optional_fields ~w(level global_drop_rarity room_destruct_message destruct_message miss_verbs hit_verbs name type worn_on weapon_type armour_type game_limit weight speed max_uses getable droppable destroy_on_death destroy_when_fully_used robbable cost_value cost_currency min_damage max_damage description)a

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

  def set_description_changeset(model, description) do
    model
    |> cast(%{description: description}, [:description])
    |> validate_required(:description)
    |> validate_length(:description, min: 20, max: 500)
  end

  def send_scroll(_item, nil), do: :ok

  def send_scroll(%Item{} = item, message) do
    case Repo.get(ItemInstance, item.instance_id) do
      %{room_id: room_id, character_id: nil} ->
        room_id
        |> RoomServer.find()
        |> send({:item_message, item.instance_id, message})

      _ ->
        PubSub.broadcast!("rooms", {:item_message, item.instance_id, message})
    end
  end

  def set_room_destruct_message_changeset(model, room_destruct_message) do
    model
    |> cast(%{room_destruct_message: room_destruct_message}, [:room_destruct_message])
    |> validate_required(:room_destruct_message)
    |> validate_length(:room_destruct_message, min: 20, max: 500)
  end

  def set_weight_changeset(model, weight) do
    model
    |> cast(%{weight: weight}, [:weight])
    |> validate_required(:weight)
    |> validate_number(:weight, greater_than_or_equal_to: 0)
  end

  def set_type_changeset(model, type) do
    model
    |> cast(%{type: type}, [:type])
    |> validate_required(:type)
    |> validate_inclusion(:type, @types)
  end

  def set_getable_changeset(model, getable) do
    model
    |> cast(%{getable: getable}, [:getable])
    |> validate_required(:getable)
  end

  def skill_for_character(%Character{} = character, %Item{type: type} = item)
      when type in ["Armour", "Shield"] do
    if owner_id = Systems.Effect.effect_bonus(item, "Claimed") do
      if owner_id == character.id do
        character.level
      else
        0
      end
    else
      type = item.armour_type

      case character.skills[type] do
        %{level: level} ->
          level

        _ ->
          0
      end
    end
  end

  # bare handed
  def skill_for_character(%Character{} = character, %Item{type: "Weapon", weapon_type: nil}),
    do: character.level

  def skill_for_character(%Character{} = character, %Item{type: "Weapon"} = item) do
    if owner_id = Systems.Effect.effect_bonus(item, "Claimed") do
      if owner_id == character.id do
        character.level
      else
        0
      end
    else
      type = item.weapon_type

      skill_level =
        case character.skills[type] do
          %{level: level} ->
            level

          _ ->
            0
        end

      class_level =
        item
        |> Systems.Effect.effect_bonus("ClassOk")
        |> Enum.map(fn class_id ->
          Enum.find(character.classes, %{level: 0}, fn character_class ->
            character_class.class_id == class_id
          end).level
        end)
        |> Enum.max(fn -> 0 end)

      max(skill_level, class_level)
    end
  end

  def skill_for_character(_character, _item), do: 1

  def str_for_item(%Item{type: "Armour"} = item) do
    str_for_armour(item.quality_level, item.armour_type)
  end

  def str_for_item(%Item{type: "Weapon"} = item) do
    str_for_weapon(item.quality_level, item.weapon_type)
  end

  def str_for_item(_item), do: 0

  def str_for_armour(nil, _armour_type), do: 0

  def str_for_armour(quality_level, armour_type) do
    10 + trunc(2.62 * quality_level * @armour_type_modifiers[armour_type])
  end

  def str_for_weapon(nil, _armour_type), do: 0

  def str_for_weapon(quality_level, "Blade") do
    trunc(1.75 * quality_level) - 1
  end

  def str_for_weapon(quality_level, "Knife") do
    str_for_weapon(quality_level, "Blade") * 0.75
  end

  def str_for_weapon(quality_level, "Staff") do
    str_for_weapon(quality_level, "Blade")
  end

  def str_for_weapon(quality_level, "Blunt") do
    str_for_weapon(quality_level, "Blade") * 1.25
  end

  def str_for_weapon(quality_level, "Two Handed Blade") do
    str_for_weapon(quality_level, "Blade") * 1.75
  end

  def str_for_weapon(quality_level, "Two Handed Blunt") do
    str_for_weapon(quality_level, "Blunt") * 1.75
  end

  def agi_for_item(%Item{type: "Weapon"} = item) do
    agi_for_weapon(item.quality_level, item.weapon_type)
  end

  def agi_for_item(_item), do: 0

  def agi_for_weapon(quality_level, "Blade") do
    str_for_weapon(quality_level, "Blade")
  end

  def agi_for_weapon(quality_level, "Knife") do
    str_for_weapon(quality_level, "Blade") * 1.25
  end

  def agi_for_weapon(quality_level, "Staff") do
    str_for_weapon(quality_level, "Blade")
  end

  def agi_for_weapon(quality_level, "Blunt") do
    agi_for_weapon(quality_level, "Blade") * 0.75
  end

  def agi_for_weapon(quality_level, "Two Handed Blade") do
    agi_for_weapon(quality_level, "Blade") * 0.75
  end

  def agi_for_weapon(quality_level, "Two Handed Blunt") do
    agi_for_weapon(quality_level, "Blunt") * 0.75
  end

  def ac_for_item(%Item{type: "Armour"} = item) do
    %{min: min, max: max} = ac_for_item(item.quality_level, item.worn_on)

    min = trunc(min * @armour_type_modifiers[item.armour_type])
    max = trunc(max * @armour_type_modifiers[item.armour_type])

    Logger.info("Quality Level #{item.quality_level} #{item.armour_type}: #{min}-#{max} ac")

    %{min: min, max: max}
  end

  def ac_for_item(_item), do: %{min: 0, max: 0}

  def ac_for_item(nil, _slot), do: %{min: 0, max: 0}

  def ac_for_item(quality, "Head") do
    max = 4 + trunc(1.9 * quality)
    min = trunc(max * 0.675)

    %{min: min, max: max}
  end

  def ac_for_item(quality, "Off-Hand") do
    max = 4 + trunc(2.23 * quality)
    min = trunc(max * 0.92) - 1

    %{min: min, max: max}
  end

  def ac_for_item(quality, type) when type in ["Feet", "Hands", "Waist", "Back"] do
    max = 1 + trunc(0.825 * quality)
    min = trunc(max * 0.875)

    %{min: min, max: max}
  end

  def ac_for_item(quality, "Torso") do
    max = 4 + trunc(7.02 * quality)
    min = trunc(max * 0.8125)

    %{min: min, max: max}
  end

  def ac_for_item(quality, "Legs") do
    max = 4 + trunc(2 * quality)
    min = trunc(max * 0.8125)

    %{min: min, max: max}
  end

  def ac_for_item(_quality, _slot), do: %{min: 0, max: 0}

  def from_assoc(%ItemInstance{id: id, item: item} = ii) do
    item =
      Repo.preload(item,
        socketable_item_affixes: [
          :item_type,
          affix: [
            affixes_traits: [:trait, :affix]
          ]
        ]
      )

    ii =
      Repo.preload(ii,
        affix_traits: [affix_trait: [:trait, :affix]],
        affix_skills: [affix_skill: [:skill, :affix]],
        sockets: [
          socketed_item: [
            item: [
              socketable_item_affixes: [
                :item_type,
                affix: [
                  affixes_traits: [:trait, :affix]
                ]
              ]
            ]
          ]
        ]
      )

    values =
      ii
      |> Map.take([
        :equipped,
        :hidden,
        :purchased,
        :owner_id,
        :delete_at,
        :uses,
        :beacon_room_id,
        :quality,
        :affix_traits,
        :affix_skills,
        :ac,
        :sockets,
        :socketable,
        :socketable_item_affixes
      ])

    values =
      if !is_nil(ii.getable) do
        Map.put(values, :getable, ii.getable)
      else
        values
      end

    item =
      item
      |> Map.merge(values)

    socketed_items =
      Enum.map(item.sockets, fn socket ->
        if socket.socketed_item_id do
          Map.put(socket, :socketed_item, Item.from_assoc(socket.socketed_item))
        else
          socket
        end
      end)

    item =
      item
      |> Map.put(:instance_id, id)
      |> Map.put(:sockets, socketed_items)
      |> load_item_types()
      |> with_traits()

    item
    |> Map.put(:uses, ii.uses || item.max_uses)
    |> Map.put(:name, ii.name || item.name)
    |> Map.put(:short_name, item.name)
    |> Map.put(:description, ii.description || item.description)
    |> Map.put(:room_destruct_message, ii.room_destruct_message || item.room_destruct_message)
    |> load_required_races_and_classes()
    |> load_item_abilities()
  end

  def from_assoc(%ShopItem{item: item}) do
    item
    |> with_traits()
    |> load_required_races_and_classes()
    |> load_item_abilities()
  end

  def load_item_types(%Item{type_id: type_id} = item) do
    item_types = item_types(type_id)

    item
    |> Map.put(:item_types, item_types)
  end

  def item_types(nil), do: []

  def item_types(type_id) do
    ItemTypeParent
    |> Ecto.Query.where(item_type_id: ^type_id)
    |> Ecto.Query.preload([:item_type])
    |> Repo.all()
    |> case do
      [] ->
        [Repo.get(ItemType, type_id)]

      parents ->
        Enum.reduce(parents, [Repo.get(ItemType, type_id)], fn parent, item_types ->
          item_types ++ item_types(parent.parent_id)
        end)
    end
    |> List.flatten()
    |> Enum.uniq()
  end

  def child_item_types(type_id) do
    ItemTypeParent
    |> Ecto.Query.where(parent_id: ^type_id)
    |> Repo.all()
    |> case do
      [] ->
        [type_id]

      children ->
        Enum.reduce(children, [type_id], fn child, item_type_ids ->
          item_type_ids ++ child_item_types(child.item_type_id)
        end)
    end
    |> List.flatten()
    |> Enum.uniq()
  end

  def delete_at(quality) when quality in ["unique", "set", "rare", "crafted"],
    do: Timex.shift(DateTime.utc_now(), minutes: 30)

  def delete_at("magic"), do: Timex.shift(DateTime.utc_now(), minutes: 5)
  def delete_at(_), do: Timex.shift(DateTime.utc_now(), minutes: 1)

  def with_traits(%Item{} = item) do
    instance_traits =
      item.instance_id
      |> ItemInstanceAffixTrait.load_traits(item)

    socket_traits =
      item.sockets
      |> Enum.reduce(%{}, fn
        %Socket{socketed_item: nil}, traits ->
          traits

        %Socket{socketed_item: %Item{} = socketed_item}, traits ->
          socketed_item.socketable_item_affixes
          |> Enum.filter(&(&1.item_type in item.item_types))
          |> List.flatten()
          |> Enum.map(& &1.affix.affixes_traits)
          |> List.flatten()
          |> Enum.reduce(traits, fn affix_trait, traits ->
            Trait.merge_traits(traits, %{affix_trait.trait.name => affix_trait.value})
          end)
      end)

    instance_skills =
      item.instance_id
      |> ItemInstanceAffixSkill.load_skills(item)

    instance_traits =
      instance_traits
      |> Trait.merge_traits(instance_skills)
      |> Trait.merge_traits(socket_traits)

    # Systems.Effect.add(item, Map.merge(item_traits, instance_traits))
    Systems.Effect.add(item, instance_traits)
  end

  def for_shop(level, item_types) do
    __MODULE__
    |> Ecto.Query.where(
      [i],
      i.quality_level <= ^level and i.quality_level >= ^level - 3 and
        i.type_id in ^item_types
    )
    |> ApathyDrive.Repo.all()
    |> case do
      [] ->
        for_shop(level - 3, item_types)

      list ->
        list
    end
  end

  def of_quality_level(level) do
    __MODULE__
    |> Ecto.Query.where(
      [i],
      i.quality_level <= ^level and i.quality_level >= ^level - 3 and i.type_id != 23
    )
    |> ApathyDrive.Repo.all()
    |> case do
      [] ->
        of_quality_level(level - 3)

      list ->
        list
    end
  end

  def gem_ids() do
    Item
    |> Ecto.Query.where(type: "Stone")
    |> Repo.all()
    |> Enum.map(& &1.id)
  end

  def random_gem(level) do
    IO.puts("dropping random gem")
    level = min(level, 18)
    ids = gem_ids()

    level =
      Enum.map(1..level, fn n ->
        Enum.map(1..n, fn _x -> level + 1 - n end)
      end)
      |> List.flatten()
      |> Enum.random()

    __MODULE__
    |> Ecto.Query.where([i], i.id in ^ids and i.quality_level <= ^level)
    |> ApathyDrive.Repo.all()
    |> Enum.random()
  end

  def match_by_name(name) do
    items =
      __MODULE__
      |> ApathyDrive.Repo.all()

    items
    |> Enum.map(&Map.put(&1, :keywords, Match.keywords(&1.name)))
    |> Match.all(:keyword_starts_with, name)
  end

  def slots do
    [
      "Arms",
      "Back",
      "Ears",
      "Feet",
      "Finger",
      "Finger",
      "Hands",
      "Head",
      "Two Handed",
      "Legs",
      "Neck",
      "Torso",
      "Waist",
      "Held",
      "Held",
      "Wrist",
      "Wrist"
    ]
  end

  def grades_for_slot(slot) do
    Item
    |> Ecto.Query.where(worn_on: ^slot)
    |> Ecto.Query.distinct(true)
    |> Ecto.Query.select([:grade])
    |> Repo.all()
    |> Enum.map(& &1.grade)
  end

  def worn_on(query, slot) do
    query |> where([item], item.worn_on == ^slot)
  end

  def grade(query, grade) do
    query |> where([item], item.grade == ^grade)
  end

  def global_drops(query) do
    query |> where([item], item.global_drop == true)
  end

  def below_level(query, level) do
    query |> where([item], item.level <= ^level)
  end

  def datalist do
    __MODULE__
    |> Repo.all()
    |> Enum.map(fn item ->
      "#{item.name} - #{item.id}"
    end)
  end

  def all do
    __MODULE__
    |> Repo.all()
  end

  def generate_for_character!(%Item{} = item, %Character{} = character, source) do
    ei = %ItemInstance{
      character_id: character.id,
      item_id: item.id,
      level: character.level,
      purchased: source == :shop
    }

    item =
      ei
      |> Map.put(:item, item)
      |> from_assoc()

    ei = Repo.insert!(ei)
    Map.put(item, :instance_id, ei.id)
  end

  def max_quality(%Item{level: level}) do
    min(div(level, 10) + 1, 5)
  end

  def color(%Item{type: type, quality_level: nil}, _opts)
      when type in ["Armour", "Shield", "Weapon"],
      do: "red"

  def color(%Item{type: type} = item, _opts)
      when type in ["Armour", "Shield", "Weapon"] do
    case item.quality do
      "unique" ->
        "#908858"

      "set" ->
        "#00c400"

      "rare" ->
        "yellow"

      "magic" ->
        "#4850B8"

      "crafted" ->
        "orange"

      "normal" ->
        "teal"

      "superior" ->
        "#FFFFFF"

      "low" ->
        "grey"

      _ ->
        "teal"
    end
  end

  def color(%Item{}, _opts), do: "teal"

  def items_to_compare(%Item{type: type} = item, %Character{} = character)
      when type in ["Armour", "Shield"] do
    worn_items = Enum.filter(character.equipment, &(&1.worn_on == item.worn_on))

    if Enum.count(worn_items) >= ApathyDrive.Commands.Wear.worn_on_max(item) do
      worn_items
    else
      []
    end
  end

  def items_to_compare(%Item{type: "Weapon"}, %Character{} = character) do
    character.equipment
    |> Enum.filter(&(&1.type == "Weapon"))
  end

  def traits(%Item{} = item, character) do
    item.effects
    |> Map.values()
    |> Enum.find(%{}, &(&1["stack_key"] == "traits"))
    |> Ability.process_duration_traits(character, character, nil)
  end

  def upgrade_for_character?(%Item{quality_level: nil}, _character), do: false
  def upgrade_for_character?(%Item{type: "Stone"}, _character), do: false

  def upgrade_for_character?(%Item{} = item, %Character{} = character) do
    unless item in character.equipment do
      qualities = %{"rare" => 3, "magic" => 2, "normal" => 1}

      case items_to_compare(item, character) do
        [] ->
          true

        items ->
          Enum.any?(items, fn worn_item ->
            item = [qualities[item.quality], item.quality_level, cost_in_copper(item)]

            worn = [
              qualities[worn_item.quality],
              worn_item.quality_level,
              cost_in_copper(worn_item)
            ]

            item > worn
          end)
      end
    end
  end

  def upgrade_for_character?(_item, _character), do: false

  def base_weapon_damage(character, weapon) do
    skill_level = Item.skill_for_character(character, weapon)

    modifier =
      if skill_level == 0 do
        0.1
      else
        skill_level / character.level
      end

    min_damage = weapon.min_damage * modifier
    max_damage = weapon.max_damage * modifier

    ability = Character.ability_for_weapon(character, weapon)

    attack_interval = Regeneration.duration_for_energy(character, max(ability.energy, 200))

    average = (min_damage + max_damage) / 2

    Float.round(average / (attack_interval / 1000), 2)
  end

  def researchable?(_item, _character), do: false

  def colored_name(item, opts \\ [])

  def colored_name(%{name: "sanctuary spell"} = _item, _opts) do
    "a <span class='white'>sanctuary</span> spell surrounding the room"
  end

  def colored_name(%{name: "asylum spell"} = _item, _opts) do
    "an <span class='dark-grey'>asylum</span> spell surrounding the room"
  end

  def colored_name(%{name: "poison rune"} = _item, _opts) do
    "<span class='green'>poison rune</span>"
  end

  def colored_name(%{name: "exploding rune"} = _item, _opts) do
    "<span class='red'>exploding rune</span>"
  end

  def colored_name(%{name: "healing rune"} = _item, _opts) do
    "<span class='cyan'>healing rune</span>"
  end

  def colored_name(%{name: "transport rune"} = _item, _opts) do
    "<span class='yellow'>transport rune</span>"
  end

  def colored_name(%{name: name} = item, opts) do
    sockets =
      item.sockets
      |> Enum.filter(&is_nil(&1.socketed_item))
      |> length()

    name =
      if item.unfinished do
        "unfinished " <> name
      else
        name
      end

    name =
      if sockets > 0 do
        name <> " (#{sockets})"
      else
        name
      end

    name = if opts[:titleize], do: titleize(name), else: name

    name =
      if pad = opts[:pad_trailing] do
        if String.length(name) >= pad do
          String.slice(name, 0..(pad - 5)) <> "... "
        else
          name
        end
      else
        name
      end

    name =
      name
      |> String.pad_trailing(opts[:pad_trailing] || 0)
      |> String.pad_leading(opts[:pad_leading] || 0)

    if opts[:character] && !opts[:no_tooltip] do
      "<span class='item-name' style='color: #{color(item, opts)};'>#{name}<span class='item tooltip'>#{ApathyDrive.Commands.Look.item_tooltip(opts[:character], item, opts)}</span></span>"
    else
      "<span style='color: #{color(item, opts)};'>#{name}</span>"
    end
  end

  def titleize(string) do
    string
    |> String.split()
    |> Enum.map(&String.capitalize/1)
    |> Enum.join(" ")
  end

  def cost_in_copper(%Item{} = item) do
    affix_bonus =
      Enum.reduce(item.affix_traits, 0, fn at, cost ->
        affix = at.affix_trait.affix
        cost + affix.level * 100
      end)

    (item.cost_value || 0) * Currency.copper_value(item.cost_currency) + affix_bonus
  end

  def has_ability?(%Item{} = item, ability_name) do
    item.effects
    |> Map.values()
    |> Enum.map(&Map.keys/1)
    |> List.flatten()
    |> Enum.member?(ability_name)
  end

  def useable_by_character?(%Character{} = character, %Item{type: "Scroll"} = scroll) do
    ability = Systems.Effect.effect_bonus(scroll, "Learn")

    !ApathyDrive.Commands.Read.wrong_class?(character, ability) and
      Ability.appropriate_alignment?(ability, character)
  end

  def useable_by_character?(%Character{}, %Item{type: "Weapon"}) do
    true
  end

  def useable_by_character?(%Character{} = character, %Item{type: type, armour_type: armour})
      when type in ["Armour", "Shield"] do
    armour_ranks = %{
      "platemail armour" => 5,
      "scalemail armour" => 4,
      "chainmail armour" => 3,
      "leather armour" => 2,
      "cloth armour" => 1
    }

    (armour_ranks[get_in(character, [:class, Access.key!(:armour)])] || 1) >=
      armour_ranks[armour]
  end

  def useable_by_character?(_character, %Item{}), do: true

  def too_powerful_for_character?(character, item) do
    too_high_level_for_character?(character, item) or not_enough_strength?(character, item) or
      not_enough_agility?(character, item)
  end

  def not_enough_strength?(character, item) do
    Mobile.attribute_at_level(character, :strength, character.level) < required_strength(item)
  end

  def not_enough_agility?(character, item) do
    Mobile.attribute_at_level(character, :agility, character.level) < required_agility(item)
  end

  def required_strength(%Item{} = item) do
    modifier = 1 + Systems.Effect.effect_bonus(item, "ReduceRequirements") / 100
    trunc(str_for_item(item) * modifier)
  end

  def required_agility(%Item{} = item) do
    modifier = 1 + Systems.Effect.effect_bonus(item, "ReduceRequirements") / 100
    trunc(agi_for_item(item) * modifier)
  end

  def required_level(%Item{} = item) do
    modifier = 1 + Systems.Effect.effect_bonus(item, "ReduceRequirements") / 100

    level =
      item.affix_traits
      |> Enum.map(& &1.affix_trait.affix.required_level)
      |> List.insert_at(0, div(item.quality_level || 0, 2))
      |> Enum.reject(&is_nil/1)
      |> Enum.max(fn -> 0 end)

    gem_level =
      if Enum.any?(item.sockets) do
        item.sockets
        |> Enum.map(fn
          %Socket{socketed_item: nil} ->
            0

          %Socket{socketed_item: %Item{} = socketed_item} ->
            socketed_item.quality_level
        end)
        |> Enum.max()
      else
        0
      end

    trunc(max(level, gem_level) * modifier)
  end

  def too_high_level_for_character?(character, %Item{type: "Scroll"} = scroll) do
    ability = Systems.Effect.effect_bonus(scroll, "Learn")

    levels =
      ApathyDrive.ClassAbility
      |> Ecto.Query.where(ability_id: ^ability.id)
      |> Repo.all()
      |> Enum.reduce(%{}, fn ca, map ->
        Map.put(map, ca.class_id, ca.level)
      end)

    !Enum.any?(character.classes, fn character_class ->
      levels[character_class.class_id] &&
        levels[character_class.class_id] <= character_class.level
    end)
  end

  def too_high_level_for_character?(character, item) do
    character.level < required_level(item)
  end

  def load_required_races_and_classes(%Item{} = item) do
    item
    |> ItemClass.load_classes()
    |> ItemRace.load_races()
  end

  def load_item_abilities(%Item{} = item) do
    item
    # |> ItemAbility.load_abilities()
    |> Enchantment.load_enchantments()
  end
end
