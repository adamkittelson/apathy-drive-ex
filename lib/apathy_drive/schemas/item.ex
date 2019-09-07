defmodule ApathyDrive.Item do
  use ApathyDriveWeb, :model

  alias ApathyDrive.{
    Ability,
    Area,
    Character,
    CharacterStyle,
    ClassAbility,
    CraftingRecipe,
    Currency,
    Enchantment,
    Item,
    ItemAbility,
    ItemClass,
    ItemInstance,
    ItemRace,
    ItemTrait,
    Mobile,
    Room,
    ShopItem,
    Trait
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
    field(:global_drop_rarity, :string)

    field(:instance_id, :integer, virtual: true)
    field(:level, :integer, virtual: true)
    field(:delete_at, :utc_datetime_usec, virtual: true)
    field(:dropped_for_character_id, :integer, virtual: true)
    field(:effects, :map, virtual: true, default: %{})
    field(:last_effect_key, :integer, virtual: true, default: 0)
    field(:timers, :map, virtual: true, default: %{})
    field(:traits, :map, virtual: true, default: %{})
    field(:required_races, :any, virtual: true, default: [])
    field(:required_classes, :any, virtual: true, default: [])
    field(:enchantments, :string, virtual: true, default: [])
    field(:keywords, :any, virtual: true)
    field(:uses, :integer, virtual: true)
    field(:hidden, :boolean, virtual: true)
    field(:unfinished, :boolean, virtual: true)

    has_many(:items_instances, ApathyDrive.ItemInstance)
  end

  @required_fields ~w(name)a
  @optional_fields ~w()a

  @armours [
    "Natural",
    "Cloth",
    "Leather",
    "Mail",
    "Plate"
  ]

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

  # set a level on placed items that have already been placed
  def from_assoc(%ItemInstance{item: %Item{type: type} = item, level: nil, room_id: room_id} = ii)
      when not is_nil(room_id) and type in ["Weapon", "Armour"] do
    traits = ItemTrait.load_traits(item.id)

    min_level = traits["MinLevel"]

    area_id =
      Room
      |> Repo.get(room_id)
      |> Map.get(:area_id)

    area_level =
      Area
      |> Repo.get(area_id)
      |> Map.get(:level)

    ii
    |> Ecto.Changeset.change(%{
      level: min_level || area_level
    })
    |> Repo.update!()
    |> from_assoc
  end

  def from_assoc(%ItemInstance{id: id, item: item} = ii) do
    values =
      ii
      |> Map.take([
        :level,
        :equipped,
        :hidden,
        :purchased,
        :dropped_for_character_id,
        :delete_at,
        :uses
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
      |> Map.put(:instance_id, id)
      |> with_traits_for_level(ii.level)

    item
    |> Map.put(:uses, ii.uses || item.max_uses)
    |> load_required_races_and_classes()
    |> load_item_abilities()
  end

  def from_assoc(%ShopItem{item: item}) do
    item
    |> with_traits_for_level(item.level)
    |> load_required_races_and_classes()
    |> load_item_abilities()
  end

  def with_traits_for_level(%Item{type: type} = item, level \\ 1) do
    item =
      if type in ["Weapon", "Armour", "Shield"] do
        Map.put(item, :level, level)
      else
        Map.put(item, :level, nil)
      end

    if recipe = CraftingRecipe.for_item(item) do
      recipe
      |> CraftingRecipe.item_with_traits(item)
    else
      item_traits = ItemTrait.load_traits(item.id)

      traits =
        if item.level, do: Map.put_new(item_traits, "MinLevel", item.level), else: item_traits

      item
      |> Map.put(:traits, traits)
    end
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
      "Off-Hand",
      "Torso",
      "Waist",
      "Weapon Hand",
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

  def price(%Item{}), do: 5

  def max_quality(%Item{level: level}) do
    min(div(level, 10) + 1, 5)
  end

  def color(%Item{type: type, traits: %{"Quality" => quality}})
      when type in ["Armour", "Shield", "Weapon"] do
    case Trait.value("Quality", quality) do
      5 ->
        "red"

      4 ->
        "olive"

      3 ->
        "darkmagenta"

      2 ->
        "blue"

      1 ->
        "chartreuse"

      _else ->
        "teal"
    end
  end

  def color(%Item{}), do: "teal"

  def enchantment(item) do
    case item.traits["Grant"] || item.traits["OnHit"] || item.traits["Passive"] do
      nil ->
        nil

      %Ability{} = ability ->
        ability.id

      [%Ability{} = ability | _rest] ->
        ability.id
    end
  end

  def upgrade_for_character?(%Item{type: "Armour"} = item, %Character{} = character) do
    unless item in character.equipment do
      original_ac = Mobile.physical_resistance_at_level(character, character.level)
      original_mr = Mobile.magical_resistance_at_level(character, character.level)

      case ApathyDrive.Commands.Wear.equip_item(character, item, false) do
        %{character: with_item} ->
          ac = Mobile.physical_resistance_at_level(with_item, with_item.level)
          mr = Mobile.magical_resistance_at_level(with_item, with_item.level)

          ac + mr > original_ac + original_mr

        _other ->
          false
      end
    end
  end

  def upgrade_for_character?(%Item{type: "Weapon"} = item, %Character{} = character) do
    unless item in character.equipment do
      %{dps: original_dps} = ApathyDrive.Commands.Look.weapon_damage(character)

      case ApathyDrive.Commands.Wear.equip_item(character, item, false) do
        %{character: with_item} ->
          %{dps: dps} = ApathyDrive.Commands.Look.weapon_damage(with_item)

          dps > original_dps

        _other ->
          false
      end
    end
  end

  def upgrade_for_character?(_item, _character), do: false

  def researchable?(%Item{} = item, %Character{} = character) do
    cond do
      !!CraftingRecipe.for_item(item) and !item.unfinished ->
        !Repo.get_by(CharacterStyle, character_id: character.id, item_id: item.id)

      !item.type == "Scroll" ->
        false

      Item.too_powerful_for_character?(character, item) ->
        false

      ApathyDrive.Commands.Read.already_learned?(character, item.traits["Learn"]) ->
        false

      ApathyDrive.Commands.Read.wrong_class?(character, item.traits["Learn"]) ->
        false

      !Ability.appropriate_alignment?(item.traits["Learn"], character) ->
        false

      :else ->
        true
    end
  end

  def researchable?(_item, _character), do: false

  def colored_name(%{name: name} = item, opts \\ []) do
    name =
      cond do
        upgrade_for_character?(item, opts[:character]) ->
          "↑ " <> name

        researchable?(item, opts[:character]) ->
          "⌕ " <> name

        :else ->
          name
      end

    name =
      if item.unfinished do
        "unfinished " <> name
      else
        name
      end

    name =
      if item.level do
        name <> "<sup> Lv" <> to_string(item.level) <> "</sup>"
      else
        name
      end

    name =
      name
      |> String.pad_trailing(opts[:pad_trailing] || 0)
      |> String.pad_leading(opts[:pad_leading] || 0)

    "<span style='color: #{color(item)};'>#{name}</span>"
  end

  def cost_in_copper(%Item{cost_currency: nil}), do: 0

  def cost_in_copper(%Item{} = item) do
    item.cost_value * Currency.copper_value(item.cost_currency)
  end

  def useable_by_character?(character, %Item{traits: %{"Learn" => ability}}) do
    class_ability =
      ClassAbility
      |> Repo.get_by(class_id: character.class_id, ability_id: ability.id)

    !!class_ability and Ability.appropriate_alignment?(ability, character)
  end

  def useable_by_character?(%Character{} = character, %Item{type: "Weapon"} = weapon) do
    cond do
      Enum.any?(weapon.required_classes) and !(character.class_id in weapon.required_classes) ->
        false

      Enum.any?(weapon.required_races) and !(character.race_id in weapon.required_races) ->
        false

      character.class_id in List.wrap(weapon.traits["ClassOk"]) ->
        true

      :else ->
        case character.weapon do
          "One Handed Blunt" ->
            weapon.weapon_type == "Blunt"

          "Two Handed Blunt" ->
            weapon.weapon_type == "Two Handed Blunt"

          "One Handed Blade" ->
            weapon.weapon_type == "Blade"

          "Two Handed Blade" ->
            weapon.weapon_type == "Two Handed Blade"

          "Any Blade" ->
            weapon.weapon_type in ["Blade", "Two Handed Blade"]

          "Any Blunt" ->
            weapon.weapon_type in ["Blunt", "Two Handed Blunt"]

          "Any One Handed" ->
            weapon.weapon_type in ["Blade", "Blunt"]

          "Any Two Handed" ->
            weapon.weapon_type in ["Two Handed Blunt", "Two Handed Blade"]

          "All" ->
            true

          "Limited" ->
            false
        end
    end
  end

  def useable_by_character?(%Character{} = character, %Item{type: type} = armour)
      when type in ["Armour", "Shield"] do
    cond do
      Enum.any?(armour.required_classes) and !(character.class_id in armour.required_classes) ->
        false

      Enum.any?(armour.required_races) and !(character.race_id in armour.required_races) ->
        false

      Enum.any?(armour.traits, fn {name, value} ->
        name == "ClassOk" and character.class_id in value
      end) ->
        true

      :else ->
        Enum.find_index(@armours, &(&1 == armour.armour_type)) <=
          Enum.find_index(@armours, &(&1 == character.armour))
    end
  end

  def useable_by_character?(_character, _item) do
    true
  end

  def too_powerful_for_character?(character, item) do
    too_high_level_for_character?(character, item) or class_too_low?(character, item)
  end

  def too_high_level_for_character?(character, item) do
    case item.traits["MinLevel"] do
      nil ->
        false

      min_level ->
        character.level < min_level
    end
  end

  def class_too_low?(character, %Item{traits: %{"Learn" => ability}}) do
    class_ability =
      ClassAbility
      |> Repo.get_by(class_id: character.class_id, ability_id: ability.id)

    if class_ability do
      class_ability.level > character.level
    end
  end

  def class_too_low?(_character, _item), do: false

  defp load_required_races_and_classes(%Item{} = item) do
    item
    |> ItemClass.load_classes()
    |> ItemRace.load_races()
  end

  defp load_item_abilities(%Item{} = item) do
    item
    |> ItemAbility.load_abilities()
    |> Enchantment.load_enchantments()
  end
end
