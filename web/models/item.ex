defmodule ApathyDrive.Item do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Character, EntityItem, Item, Mobile}

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

  @required_fields ~w(name description weight worn_on level grade)
  @optional_fields ~w(abilities global_drop)
  @rarities %{
    "common" => %{
      cost_multiplier: 1,
      color: "darkgrey",
      attribute_range: 2..4,
      base_power: 18
    },
    "uncommon" => %{
      cost_multiplier: 2,
      color: "teal",
      attribute_range: 5..7,
      base_power: 36
    },
    "rare" => %{
      cost_multiplier: 3,
      color: "#0070ff",
      attribute_range: 8..10,
      base_power: 54
    },
    "epic" => %{
      cost_multiplier: 4,
      color: "#a335ee",
      attribute_range: 11..19,
      base_power: 90
    },
    "legendary" => %{
      cost_multiplier: :infinity,
      color: "#ff8000",
      attribute_range: 20..28,
      base_power: 144
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

  def attribute_for_character(%Item{rarity: "legendary"} = item, %Character{} = character, attribute) do
    attribute_at_level(item, character.level, attribute)
  end
  def attribute_for_character(%Item{} = item, %Character{} = character, attribute) do
    level = min(item.level, character.level)
    attribute_at_level(item, level, attribute)
  end

  def attribute_at_level(%Item{} = item, level, attribute) do
    min..max = @rarities[item.rarity].attribute_range
    average = div(min + max, 2)

    base = Map.get(item, attribute) || average
    base + ((base / 10) * (level - 1))
  end

  def power_at_level(%Item{} = item, level) do
    base = @rarities[item.rarity].base_power
    base + ((base / 10) * (level - 1))
  end
  def power_at_level(rarity, level) do
    power_at_level(%Item{rarity: rarity}, level)
  end

  def slots do
    ["Arms", "Back", "Ears", "Feet", "Finger", "Finger", "Hands", "Head", "Two Handed",
     "Legs", "Neck", "Off-Hand", "Torso", "Waist", "Weapon Hand", "Wrist", "Wrist"]
  end

  def slots_below_power(%Character{equipment: equipment, level: level} = character, power) do
    upgradeable_slots =
      slots()
      |> Enum.reduce(equipment, fn slot, mapped_equipment ->
           item =
             Enum.find(mapped_equipment, fn
               %Item{worn_on: worn_on} = item when worn_on == slot ->
                 item
               _ ->
                 nil
             end)

           if item do
             mapped_equipment =
               mapped_equipment
               |> List.delete(item)

             [{slot, power_at_level(item, level)} | mapped_equipment]
           else
             [{slot, 0} | mapped_equipment]
           end
         end)
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

  def generate_item_attributes(rarity, :purchased) do
    min..max = @rarities[rarity].attribute_range

    average = div(min + max, 2)

    %{strength: average,
      agility: average,
      intellect: average,
      willpower: average,
      health: average,
      charm: average
    }
  end

  def generate_item_attributes(rarity, :looted) do
    min..max = @rarities[rarity].attribute_range

    [:strength, :agility, :intellect, :willpower, :health, :charm]
    |> Enum.shuffle
    |> Enum.chunk(2)
    |> Enum.reduce(%{}, fn [attribute_1, attribute_2], attributes ->
         roll = Enum.random(min..max)
         attributes
         |> Map.put(attribute_1, roll)
         |> Map.put(attribute_2, max - roll + min)
       end)
  end

  def generate_for_character!(%Item{rarity: rarity} = item, %Character{} = character, source) do
    %EntityItem{
      assoc_table: "characters",
      assoc_id: character.id,
      item_id: item.id,
      level: character.level
    }
    |> Map.merge(generate_item_attributes(rarity, source))
    |> Repo.insert!

    item
  end

  def generate_item(%{chance: chance, item_id: _item_id, level: _level} = opts) do
    if :rand.uniform(100) <= chance do
      opts
      |> Map.delete(:chance)
      |> generate_item
    end
  end

  def generate_item(%{item_id: :global, level: level}) do
    # item_id = random_item_id_below_level(level)
    # generate_item(%{item_id: item_id, level: level})
  end

  def generate_item(%{item_id: item_id, level: level}) do
    Repo.get(__MODULE__, item_id)
    |> to_map
    |> roll_stats(level)
  end

  def to_map(nil), do: nil
  def to_map(%__MODULE__{} = item) do
    item
    |> Map.from_struct
    |> Map.take([:name, :description, :weight, :worn_on,
                 :level, :strength, :agility, :will, :grade, :abilities, :id])
    |> Poison.encode! # dirty hack to
    |> Poison.decode! # stringify the keys
  end

  def roll_stats(nil, _rolls),   do: nil
  def roll_stats(%{} = item, 0), do: item
  def roll_stats(%{} = item, rolls) do
    if :rand.uniform(10) > 7 do
      item
      |> enhance
      |> roll_stats(rolls)
    else
      roll_stats(item, rolls - 1)
    end
  end

  def enhance(item) do
    str = strength(item)
    agi = agility(item)
    will = will(item)

    case :rand.uniform(str + agi + will) do
      roll when roll > (str + agi) ->
        Map.put(item, "will", will + 1)
      roll when roll <= str ->
        Map.put(item, "strength", str + 1)
      _ ->
        Map.put(item, "agility", agi + 1)
    end
  end

  def deconstruction_experience(item) do
    str = strength(item)
    agi = agility(item)
    will = will(item)

    experience(str + agi + will)
  end

  def experience(num) do
    (0..num)
    |> Enum.reduce(0, fn(n, total) ->
         total + n
       end)
  end

  def strength(%{level: level, grade: "light"}),            do: 1 + div(level, 2)
  def strength(%{level: level, grade: "medium"}),           do: 1 + div(level, 2)
  def strength(%{level: level, grade: "heavy"}),            do: 2 + level
  def strength(%{level: level, grade: "blunt"}),            do: 1 + div(level, 2)
  def strength(%{level: level, grade: "blade"}),            do: 1 + div(level, 2)
  def strength(%{level: level, grade: "two handed blunt"}), do: 2 + level
  def strength(%{level: level, grade: "two handed blade"}), do: 2 + level
  def strength(%{"strength" => str}),                       do: str
  def strength(%{"level" => level, "grade" => grade}),      do: strength(%{level: level, grade: grade})

  def agility(%{level: level, grade: "light"}),            do: 1 + div(level, 2)
  def agility(%{level: level, grade: "medium"}),           do: 2 + level
  def agility(%{level: level, grade: "heavy"}),            do: 1 + div(level, 2)
  def agility(%{level: level, grade: "blunt"}),            do: 1 + div(level, 2)
  def agility(%{level: level, grade: "blade"}),            do: 1 + div(level, 2)
  def agility(%{level: level, grade: "two handed blunt"}), do: trunc(0.5 + div(level, 4))
  def agility(%{level: level, grade: "two handed blade"}), do: trunc(0.5 + div(level, 4))
  def agility(%{"agility" => agi}),                        do: agi
  def agility(%{"level" => level, "grade" => grade}),      do: agility(%{level: level, grade: grade})

  def will(%{level: level, grade: "light"}),            do: 2 + level
  def will(%{level: level, grade: "medium"}),           do: 1 + div(level, 2)
  def will(%{level: level, grade: "heavy"}),            do: 1 + div(level, 2)
  def will(%{level: level, grade: "blunt"}),            do: 1 + div(level, 2)
  def will(%{level: level, grade: "blade"}),            do: 1 + div(level, 2)
  def will(%{level: level, grade: "two handed blunt"}), do: 1 + div(level, 2)
  def will(%{level: level, grade: "two handed blade"}), do: 1 + div(level, 2)
  def will(%{"will" => will}),                          do: will
  def will(%{"level" => level, "grade" => grade}),      do: will(%{level: level, grade: grade})

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
