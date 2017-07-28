defmodule ApathyDrive.Item do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Character, EntityItem, Item, Mobile}
  require Logger

  schema "items" do
    field :name, :string
    field :description, :string
    field :worn_on, :string
    field :grade, :string
    field :global_drop, :boolean
    field :game_limit, :integer
    field :rarity, :string
    field :hit_verbs, ApathyDrive.JSONB
    field :miss_verbs, ApathyDrive.JSONB
    field :abilities, :map, virtual: true, default: %{}
    field :level, :integer, virtual: true
    field :equipped, :boolean, virtual: true, default: false
    field :strength, :integer, virtual: true
    field :agility, :integer, virtual: true
    field :intellect, :integer, virtual: true
    field :willpower, :integer, virtual: true
    field :health, :integer, virtual: true
    field :charm, :integer, virtual: true
    field :entities_items_id, :integer, virtual: true

    has_many :shop_items, ApathyDrive.ShopItem
    has_many :shops, through: [:shop_items, :room]

    has_many :characters_items, ApathyDrive.EntityItem
    has_many :characters, through: [:characters_items, :character]

    has_many :items_abilities, ApathyDrive.ItemAbility

    timestamps
  end

  @required_fields ~w(name description worn_on level grade)
  @optional_fields ~w(abilities global_drop)
  @rarities %{
    "common" => %{
      cost_multiplier: 1,
      color: "teal",
      attributes: 3
    },
    "uncommon" => %{
      cost_multiplier: 2,
      color: "chartreuse",
      attributes: 6
    },
    "rare" => %{
      cost_multiplier: 3,
      color: "blue",
      attributes: 9
    },
    "epic" => %{
      cost_multiplier: 4,
      color: "darkmagenta",
      attributes: 15
    },
    "legendary" => %{
      cost_multiplier: :infinity,
      color: "red",
      attributes: 24
    }
  }

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  def changeset(model, params \\ %{}) do
    model
    |> cast(params, @required_fields, @optional_fields)
  end

  def from_assoc(%EntityItem{id: id, item: item} = ei) do
    values =
      Map.take(ei, [:level, :equipped, :strength, :agility,
                    :intellect, :willpower, :health, :charm])

    Map.merge(item, values)
    |> Map.put(:entities_items_id, id)
  end

  def attribute_at_level(%Item{} = item, level, attribute) do
    level = min(item.level, level)
    average = @rarities[item.rarity].attributes

    growth =
      [:strength, :agility, :intellect, :willpower, :health, :charm]
      |> Enum.reduce(0, & &2 + (Map.get(item, &1) || average))
      |> div(6)

    base = Map.get(item, attribute) || average

    base + ((growth / 10) * (level - 1))
  end

  def power_at_level(%Item{} = item, level) do
    base = @rarities[item.rarity].attributes * 6
    base + ((base / 10) * (level - 1))
  end
  def power_at_level(rarity, level) do
    power_at_level(%Item{rarity: rarity}, level)
  end

  def slots do
    ["Arms", "Back", "Ears", "Feet", "Finger", "Finger", "Hands", "Head", "Two Handed",
     "Legs", "Neck", "Off-Hand", "Torso", "Waist", "Weapon Hand", "Wrist", "Wrist"]
  end

  def slots_below_power(%Character{equipment: equipment, inventory: inventory} = character, power) do
    upgradeable_slots =
      slots()
      |> Enum.reduce(equipment ++ inventory, fn slot, mapped_equipment ->
           items =
             Enum.filter(mapped_equipment, fn
               %Item{worn_on: worn_on} = item when worn_on == slot ->
                 item
               _ ->
                 false
             end)

           if Enum.any?(items) do
             item = Enum.max_by(items, &power_at_level(&1, &1.level))

             mapped_equipment =
               mapped_equipment
               |> List.delete(item)

             [{slot, power_at_level(item, item.level)} | mapped_equipment]
           else
             [{slot, 0} | mapped_equipment]
           end
         end)
      |> Enum.filter(&is_tuple/1)
      |> Enum.reject(&(elem(&1, 1) >= power))
      |> Enum.map(&(elem(&1, 0)))

    case Character.weapon(character) do
      nil ->
        upgradeable_slots
      %Item{worn_on: "Weapon Hand"} ->
        Enum.reject(upgradeable_slots, &(&1 == "Two Handed"))
      %Item{worn_on: "Two Handed"} ->
        Enum.reject(upgradeable_slots, &(&1 in ["Weapon Hand", "Off-Hand"]))
    end
  end

  def grade_for_character(%Character{weapon_type: type}, slot) when slot in ["Weapon Hand", "Two Handed"] and type != "Any" do
    type
  end
  def grade_for_character(%Character{}, slot) when slot in ["Weapon Hand", "Two Handed"], do: nil
  def grade_for_character(%Character{armour: grade}, _slot), do: grade

  def random_item_id_for_slot_and_rarity(%Character{} = character, slot, rarity) do
    grade = grade_for_character(character, slot)

    if grade do
      random_item_id_for_grade_and_slot_and_rarity(grade, slot, rarity)
    else
      count =
        __MODULE__
        |> worn_on(slot)
        |> rarity(rarity)
        |> select([item], count(item.id))
        |> Repo.one

      __MODULE__
      |> worn_on(slot)
      |> rarity(rarity)
      |> offset(fragment("floor(random()*?) LIMIT 1", ^count))
      |> select([item], item.id)
      |> Repo.one
    end
  end

  def random_item_id_for_grade_and_slot_and_rarity(grade, slot, rarity) do
    Logger.info "finding random #{inspect rarity} item with grade: #{inspect grade} for slot: #{inspect slot}"
    count =
      __MODULE__
      |> grade(grade)
      |> worn_on(slot)
      |> rarity(rarity)
      |> select([item], count(item.id))
      |> Repo.one

    cond do
      count > 0 ->
        __MODULE__
        |> grade(grade)
        |> worn_on(slot)
        |> rarity(rarity)
        |> offset(fragment("floor(random()*?) LIMIT 1", ^count))
        |> select([item], item.id)
        |> Repo.one
      grade == "Plate" ->
        random_item_id_for_grade_and_slot_and_rarity("Scale", slot, rarity)
      grade == "Scale" ->
        random_item_id_for_grade_and_slot_and_rarity("Chain", slot, rarity)
      grade == "Chain" ->
        random_item_id_for_grade_and_slot_and_rarity("Leather", slot, rarity)
      grade == "Leather" ->
        random_item_id_for_grade_and_slot_and_rarity("Cloth", slot, rarity)
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
    query |> where([item], item.level <= ^level )
  end

  def datalist do
    __MODULE__
    |> Repo.all
    |> Enum.map(fn(item) ->
         "#{item.name} - #{item.id}"
       end)
  end

  def all do
    __MODULE__
    |> Repo.all
  end

  def generate_item_attributes(rarity) do
    %{strength: @rarities[rarity].attributes,
      agility: @rarities[rarity].attributes,
      intellect: @rarities[rarity].attributes,
      willpower: @rarities[rarity].attributes,
      health: @rarities[rarity].attributes,
      charm: @rarities[rarity].attributes
    }
  end

 def generate_for_character!(%Item{rarity: rarity} = item, %Character{} = character, persist?) do
    ei =
      %EntityItem{
        assoc_table: "characters",
        assoc_id: character.id,
        item_id: item.id,
        level: (if rarity == "legendary", do: :infinity, else: character.level)
      }
      |> Map.merge(generate_item_attributes(rarity))

    ei = if persist?, do: Repo.insert!(ei), else: ei

    ei
    |> Map.put(:item, item)
    |> from_assoc()
  end

  def price(%Item{rarity: "legendary"}), do: "priceless"
  def price(%Item{rarity: rarity, level: level} = item) do
    attributes =
      [:strength, :agility, :intellect, :willpower, :health, :charm]
      |> Enum.reduce(0, fn attribute, total ->
           total + attribute_at_level(item, level, attribute)
         end)

    trunc((attributes * @rarities[rarity].cost_multiplier))
  end

  def color(%Item{rarity: rarity}) do
    @rarities[rarity].color
  end

  def colored_name(%Item{name: name} = item, opts \\ []) do
    name =
      name
      |> String.ljust(opts[:ljust] || 0)
      |> String.rjust(opts[:rjust] || 0)

    "<span style='color: #{color(item)}'>#{name}</span>"
  end

end
