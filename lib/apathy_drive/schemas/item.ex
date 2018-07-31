defmodule ApathyDrive.Item do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Character, Enchantment, Item, ItemInstance}
  alias ApathyDrive.Items.{Weapon}
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
    field(:required_strength, :integer)
    field(:max_uses, :integer)
    field(:getable, :boolean)
    field(:dropable, :boolean)
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

    field(:instance_id, :integer, virtual: true)

    has_many(:items_instances, ApathyDrive.ItemInstance)

    timestamps()
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

  def from_assoc(%ItemInstance{id: id, item: item} = ii) do
    values =
      ii
      |> Map.take([:level, :equipped, :hidden, :purchased])

    item
    |> Map.merge(values)
    |> Map.put(:instance_id, id)
    |> case do
      %Item{type: "Weapon"} = item ->
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

  def color(%Item{}) do
    "dark-cyan"
  end

  def colored_name(%{name: name}, opts \\ []) do
    name =
      name
      |> String.pad_trailing(opts[:pad_trailing] || 0)
      |> String.pad_leading(opts[:pad_leading] || 0)

    "<span style='color: dark-cyan;'>#{name}</span>"
  end
end
