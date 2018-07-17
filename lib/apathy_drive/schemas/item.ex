defmodule ApathyDrive.Item do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Character, Enchantment, Item, ItemInstance}
  alias ApathyDrive.Items.{Weapon}
  require Logger
  require Ecto.Query

  schema "items" do
    field(:name, :string)
    field(:description, :string)
    field(:worn_on, :string)
    field(:grade, :string)
    field(:global_drop, :boolean)
    field(:game_limit, :integer)
    field(:rarity, :string)
    field(:hit_verbs, ApathyDrive.JSONB)
    field(:miss_verbs, ApathyDrive.JSONB)
    field(:attacks_per_round, :integer)
    field(:physical_resistance, :integer)
    field(:magical_resistance, :integer)
    field(:kind, :string)
    field(:weight, :integer)
    field(:speed, :integer)
    field(:min_damage, :integer)
    field(:max_damage, :integer)
    field(:required_strength, :integer)
    field(:required_agility, :integer)
    field(:required_intellect, :integer)
    field(:required_willpower, :integer)
    field(:required_health, :integer)
    field(:required_charm, :integer)

    field(:abilities, :map, virtual: true, default: %{})
    field(:level, :integer, virtual: true)
    field(:equipped, :boolean, virtual: true, default: false)
    field(:hidden, :boolean, virtual: true, default: false)
    field(:instance_id, :integer, virtual: true)
    field(:damage, :any, virtual: true)
    field(:effects, :map, virtual: true, default: %{})
    field(:last_effect_key, :integer, virtual: true, default: 0)
    field(:purchased, :boolean, virtual: true, default: false)

    has_many(:shop_items, ApathyDrive.ShopItem)
    has_many(:shops, through: [:shop_items, :room])

    has_many(:items_instances, ApathyDrive.ItemInstance)
    has_many(:rooms, through: [:items_instances, :room])
    has_many(:characters, through: [:items_instances, :character])

    has_many(:items_abilities, ApathyDrive.ItemAbility)

    timestamps()
  end

  @required_fields ~w(name description worn_on level grade)a
  @optional_fields ~w(abilities global_drop)a
  @rarities %{
    "common" => %{
      multiplier: 1,
      color: "teal"
    },
    "uncommon" => %{
      multiplier: 2,
      color: "chartreuse"
    },
    "rare" => %{
      multiplier: 3,
      color: "blue"
    },
    "epic" => %{
      multiplier: 5,
      color: "darkmagenta"
    },
    "legendary" => %{
      multiplier: 8,
      color: "red"
    }
  }

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
      |> Map.take([:level, :equipped, :hidden, :purchased])

    item
    |> Map.merge(values)
    |> Map.put(:instance_id, id)
    |> case do
      %Item{kind: "weapon"} = item ->
        %Weapon{
          name: item.name,
          description: item.description,
          worn_on: item.worn_on,
          kind: item.grade,
          hit_verbs: item.hit_verbs,
          miss_verbs: item.miss_verbs,
          instance_id: item.instance_id,
          weight: item.weight,
          speed: item.speed,
          min_damage: item.min_damage,
          max_damage: item.max_damage,
          required_strength: item.required_strength,
          required_agility: item.required_agility,
          required_intellect: item.required_intellect,
          required_willpower: item.required_willpower,
          required_health: item.required_health,
          required_charm: item.required_charm,
          equipped: item.equipped,
          effects: item.effects
        }

      %Item{} = item ->
        item
        |> Enchantment.load_enchantments()
    end
  end

  def from_shop(%Item{} = item) do
    item
  end

  def power_at_level(%Item{} = item, level) do
    base = 3 * @rarities[item.rarity].multiplier * 6
    base + base / 10 * (level - 1)
  end

  def power_at_level(rarity, level) do
    power_at_level(%Item{rarity: rarity}, level)
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

  def random_item_id_for_slot_and_rarity(slot, rarity) do
    grade =
      slot
      |> grades_for_slot()
      |> Enum.random()

    if grade do
      random_item_id_for_grade_and_slot_and_rarity(grade, slot, rarity)
    end
  end

  def random_item_id_for_grade_and_slot_and_rarity(grade, slot, rarity) do
    Logger.info(
      "finding random #{inspect(rarity)} item with grade: #{inspect(grade)} for slot: #{
        inspect(slot)
      }"
    )

    count =
      __MODULE__
      |> grade(grade)
      |> worn_on(slot)
      |> rarity(rarity)
      |> select([item], count(item.id))
      |> Repo.one()

    if count > 0 do
      __MODULE__
      |> grade(grade)
      |> worn_on(slot)
      |> rarity(rarity)
      |> offset(fragment("floor(random()*?) LIMIT 1", ^count))
      |> select([item], item.id)
      |> Repo.one()
    else
      Logger.error("Tried to spawn item for #{grade} #{slot} #{rarity}, but none exist.")
    end
  end

  def worn_on(query, slot) do
    query |> where([item], item.worn_on == ^slot)
  end

  def grade(query, grade) do
    query |> where([item], item.grade == ^grade)
  end

  def rarity(query, rarity) do
    query |> where([item], item.rarity == ^rarity)
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

    if source == :loot and !upgrade_for_character?(item, character) do
      item
    else
      ei = Repo.insert!(ei)
      Map.put(item, :instance_id, ei.id)
    end
  end

  def upgrade_for_character?(%Item{worn_on: slot, rarity: rarity, level: level}, %Character{
        equipment: equipment
      }) do
    item_power = Item.power_at_level(rarity, level)

    worn_item =
      Enum.find(equipment, fn worn_item ->
        cond do
          slot in ["Off-Hand", "Weapon Hand"] ->
            worn_item.worn_on == slot or worn_item.worn_on == "Two Handed"

          slot == "Two Handed" ->
            worn_item.worn_on == slot or worn_item.worn_on == "Weapon Hand" or
              worn_item.worn_on == "Off-Hand"

          :else ->
            worn_item.worn_on == slot
        end
      end)

    slot_power =
      if worn_item do
        Item.power_at_level(worn_item.rarity, worn_item.level)
      else
        0
      end

    item_power > slot_power
  end

  def price(%Item{rarity: "legendary"}), do: "priceless"

  def price(%Item{}), do: 5

  def color(%Item{rarity: rarity}) do
    @rarities[rarity].color
  end

  def colored_name(%{name: name}, opts \\ []) do
    name =
      name
      |> String.pad_trailing(opts[:pad_trailing] || 0)
      |> String.pad_leading(opts[:pad_leading] || 0)

    "<span style='color: dark-cyan;'>#{name}</span>"
  end
end
