defmodule ApathyDrive.Item do
  use ApathyDriveWeb, :model

  alias ApathyDrive.{
    Character,
    ClassAbility,
    Currency,
    Enchantment,
    Item,
    ItemAbility,
    ItemClass,
    ItemInstance,
    ItemRace,
    ItemTrait,
    Mobile,
    ShopItem
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
    field(:required_strength, :integer)

    field(:instance_id, :integer, virtual: true)
    field(:delete_at, :utc_datetime_usec, virtual: true)
    field(:dropped_for_character_id, :integer, virtual: true)
    field(:effects, :map, virtual: true, default: %{})
    field(:traits, :map, virtual: true, default: %{})
    field(:required_races, :any, virtual: true, default: [])
    field(:required_classes, :any, virtual: true, default: [])
    field(:enchantment_name, :string, virtual: true, default: nil)
    field(:keywords, :any, virtual: true)
    field(:uses, :integer, virtual: true)
    field(:hidden, :boolean, virtual: true)

    has_many(:items_instances, ApathyDrive.ItemInstance)
  end

  @required_fields ~w(name)a
  @optional_fields ~w()a

  @armours [
    "Natural",
    "Robes",
    "Padded",
    "Soft Leather",
    "Soft Studded Leather",
    "Rigid Leather",
    "Studded Rigid Leather",
    "Chainmail",
    "Scalemail",
    "Platemail"
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

    item
    |> Map.merge(values)
    |> Map.put(:instance_id, id)
    |> Map.put(:traits, ItemTrait.load_traits(item.id))
    |> Map.put(:uses, ii.uses || item.max_uses)
    |> load_required_races_and_classes()
    |> load_item_abilities()
  end

  def from_assoc(%ShopItem{item: item}) do
    item
    |> Map.put(:traits, ItemTrait.load_traits(item.id))
    |> load_required_races_and_classes()
    |> load_item_abilities()
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

  def color(%Item{traits: %{"Quality" => 5}}), do: "red"
  def color(%Item{traits: %{"Quality" => 4}}), do: "olive"
  def color(%Item{traits: %{"Quality" => 3}}), do: "darkmagenta"
  def color(%Item{traits: %{"Quality" => 2}}), do: "blue"
  def color(%Item{traits: %{"Quality" => 1}}), do: "chartreuse"
  def color(%Item{traits: %{"Quality" => [5]}}), do: "red"
  def color(%Item{traits: %{"Quality" => [4]}}), do: "olive"
  def color(%Item{traits: %{"Quality" => [3]}}), do: "darkmagenta"
  def color(%Item{traits: %{"Quality" => [2]}}), do: "blue"
  def color(%Item{traits: %{"Quality" => [1]}}), do: "chartreuse"
  def color(%Item{}), do: "teal"

  def enchantment(item) do
    enchantment = item.traits["Grant"] || item.traits["OnHit"] || item.traits["Passive"]
    enchantment && enchantment.id
  end

  def colored_name(%{name: name} = item, opts \\ []) do
    name =
      if item.enchantment_name do
        name <> " of " <> "#{item.enchantment_name |> String.split("song of ") |> List.last()}"
      else
        name
      end
      |> String.pad_trailing(opts[:pad_trailing] || 0)
      |> String.pad_leading(opts[:pad_leading] || 0)

    "<span style='color: #{color(item)};'>#{name}</span>"
  end

  def cost_in_copper(%Item{cost_currency: nil}), do: 0

  def cost_in_copper(%Item{} = item) do
    item.cost_value * Currency.copper_value(item.cost_currency)
  end

  def useable_by_character?(%Character{} = character, %Item{type: "Weapon"} = weapon) do
    cond do
      Enum.any?(weapon.required_classes) and !(character.class_id in weapon.required_classes) ->
        false

      Enum.any?(weapon.required_races) and !(character.race_id in weapon.required_races) ->
        false

      Enum.any?(weapon.traits, fn {name, value} ->
        name == "ClassOk" and character.class_id in value
      end) ->
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

  def useable_by_character?(_character, _item), do: true

  def too_powerful_for_character?(character, item) do
    too_high_level_for_character?(character, item) or
      attribute_requirement_not_met?(character, item)
  end

  def too_high_level_for_character?(character, item) do
    case item.traits["MinLevel"] do
      [min_level | []] ->
        character.level < min_level

      _ ->
        false
    end
  end

  def attribute_requirement_not_met?(character, %Item{traits: %{"Learn" => ability}}) do
    attribute_not_met? =
      Enum.any?(ability.attributes, fn {attribute, value} ->
        Mobile.attribute_at_level(character, attribute, character.level) < value
      end)

    class_ability =
      ClassAbility
      |> Repo.get_by(class_id: character.class_id, ability_id: ability.id)

    if class_ability do
      class_ability.level > character.level and attribute_not_met?
    else
      attribute_not_met?
    end
  end

  def attribute_requirement_not_met?(_character, _item), do: false

  defp load_required_races_and_classes(%Item{} = item) do
    item
    |> ItemClass.load_classes()
    |> ItemRace.load_races()
  end

  defp load_item_abilities(%Item{} = item) do
    item
    |> ItemAbility.load_abilities()
    |> Enchantment.load_enchantment()
  end
end
