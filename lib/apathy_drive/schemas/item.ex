defmodule ApathyDrive.Item do
  use ApathyDriveWeb, :model

  alias ApathyDrive.{
    Ability,
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

  @weapon_type_modifier %{
    "blunt" => 1.0,
    "blade" => 1.1,
    "two handed blunt" => 1.15,
    "two handed blade" => 1.25
  }

  @armour_type_protection %{
    "cloth armour" => 0.10,
    "leather armour" => 0.15,
    "chainmail armour" => 0.20,
    "scalemail armour" => 0.25,
    "platemail armour" => 0.30
  }

  @slot_physical_protection_modifier %{
    "Head" => 0.15,
    "Torso" => 0.15,
    "Arm" => 0.1,
    "Back" => 0.1,
    "Foot" => 0.1,
    "Hand" => 0.1,
    "Legs" => 0.15,
    "Held" => 0.1,
    "Waist" => 0.05,
    "Ears" => 0.0,
    "Finger" => 0.0,
    "Neck" => 0.0,
    "Wrist" => 0.0,
    "Worn" => 0.0
  }

  @slot_magical_protection_modifier %{
    "Head" => 0.0,
    "Torso" => 0.0,
    "Arm" => 0.0,
    "Back" => 0.0,
    "Foot" => 0.0,
    "Hand" => 0.0,
    "Legs" => 0.0,
    "Held" => 0.0,
    "Waist" => 0.0,
    "Ears" => 0.3,
    "Finger" => 0.2,
    "Neck" => 0.3,
    "Wrist" => 0.2,
    "Worn" => 0.0
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

    field(:shatter_chance, :float, virtual: true, default: 0.0)
    field(:limb, :string, virtual: true)
    field(:instance_id, :integer, virtual: true)
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

  def target_damage(0), do: 0
  def target_damage(1), do: 6

  def target_damage(level) do
    trunc(round(target_damage(level - 1) + max(1, (level - 1) / 6)))
  end

  def target_damage(weapon_type, skill_level) do
    trunc(target_damage(skill_level) * @weapon_type_modifier[weapon_type])
  end

  def ac_for_character(%Character{} = character, %Item{type: "Armour"} = item) do
    type = item.armour_type

    skill_level =
      case character.skills[type] do
        %{level: level} ->
          level

        _ ->
          0
      end

    ac(type, skill_level, item.worn_on)
  end

  def ac_for_character(_character, _item), do: 0

  def mr_for_character(%Character{} = character, %Item{type: "Armour"} = item) do
    type = item.armour_type

    skill_level =
      case character.skills[type] do
        %{level: level} ->
          level

        _ ->
          0
      end

    mr(type, skill_level, item.worn_on)
  end

  def mr_for_character(_character, _item), do: 0

  def ac(type, level, slot) do
    mitigation = @armour_type_protection[type]
    base = -(50 * level * mitigation / (mitigation - 1))
    max(1, trunc(base * @slot_physical_protection_modifier[slot]))
  end

  def mr(type, level, slot) do
    mitigation = @armour_type_protection[type]
    base = -(50 * level * mitigation / (mitigation - 1))
    max(1, trunc(base * @slot_magical_protection_modifier[slot]))
  end

  def from_assoc(%ItemInstance{id: id, item: item} = ii) do
    values =
      ii
      |> Map.take([
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
      |> with_traits()

    item
    |> Map.put(:uses, ii.uses || item.max_uses)
    |> Map.put(:name, ii.name || item.name)
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

  def with_traits(%Item{} = item) do
    item_traits = ItemTrait.load_traits(item.id)

    item
    |> Map.put(:traits, item_traits)
  end

  def slots do
    [
      "Arm",
      "Arm",
      "Back",
      "Ears",
      "Foot",
      "Foot",
      "Finger",
      "Finger",
      "Hand",
      "Hand",
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

  def price(%Item{}), do: 5

  def max_quality(%Item{level: level}) do
    min(div(level, 10) + 1, 5)
  end

  def color(%Item{type: type} = item, opts)
      when type in ["Armour", "Shield", "Weapon"] do
    shatter_chance = Enchantment.shatter_chance(opts[:character], item)

    cond do
      shatter_chance > 0.25 ->
        "darkmagenta"

      shatter_chance > 0.1 ->
        "blue"

      shatter_chance > 0 ->
        "chartreuse"

      :else ->
        "teal"
    end
  end

  def color(%Item{}, _opts), do: "teal"

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

  def researchable?(_item, _character), do: false

  def colored_name(item, opts \\ [])

  def colored_name(%{name: "sanctuary spell"} = _item, _opts) do
    "a <span class='white'>sanctuary</span> spell surrounding the room"
  end

  def colored_name(%{name: "asylum spell"} = _item, _opts) do
    "an <span class='dark-grey'>asylum</span> spell surrounding the room"
  end

  def colored_name(%{name: name} = item, opts) do
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
      name
      |> String.pad_trailing(opts[:pad_trailing] || 0)
      |> String.pad_leading(opts[:pad_leading] || 0)

    "<span style='color: #{color(item, opts)};'>#{name}</span>"
  end

  def cost_in_copper(%Item{cost_currency: nil}), do: 0

  def cost_in_copper(%Item{} = item) do
    item.cost_value * Currency.copper_value(item.cost_currency)
  end

  def useable_by_character?(_character, %Item{traits: %{"Learn" => _ability}}), do: false

  def useable_by_character?(%Character{} = character, %Item{type: "Weapon"} = weapon) do
    class_ids =
      character.classes
      |> Enum.map(& &1.class_id)
      |> Enum.into(MapSet.new())

    required_classes = Enum.into(weapon.required_classes, MapSet.new())

    cond do
      Enum.any?(required_classes) and !Enum.any?(MapSet.intersection(class_ids, required_classes)) ->
        false

      Enum.any?(weapon.required_races) and !(character.race_id in weapon.required_races) ->
        false

      :else ->
        true
    end
  end

  def useable_by_character?(%Character{} = character, %Item{type: type} = armour)
      when type in ["Armour", "Shield"] do
    class_ids =
      character.classes
      |> Enum.map(& &1.class_id)
      |> Enum.into(MapSet.new())

    required_classes = Enum.into(armour.required_classes, MapSet.new())

    cond do
      Enum.any?(armour.required_races) and !(character.race_id in armour.required_races) ->
        false

      Enum.any?(armour.required_races) and
          !Enum.any?(MapSet.intersection(class_ids, required_classes)) ->
        false

      :else ->
        true
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
