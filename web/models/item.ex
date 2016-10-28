defmodule ApathyDrive.Item do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Character, Item, Mobile}

  schema "items" do
    field :name, :string
    field :description, :string
    field :worn_on, :string
    field :grade, :string
    field :abilities, ApathyDrive.JSONB
    field :global_drop, :boolean
    field :game_limit, :integer
    field :rarity, :string
    field :strength, :integer
    field :agility, :integer
    field :intellect, :integer
    field :willpower, :integer
    field :health, :integer
    field :charm, :integer

    has_many :shop_items, ApathyDrive.ShopItem
    has_many :shops, through: [:shop_items, :room]

    has_many :characters_items, ApathyDrive.CharacterItem
    has_many :characters, through: [:characters_items, :character]

    has_many :rooms_items, ApathyDrive.RoomItem
    has_many :rooms, through: [:rooms_items, :room]

    timestamps
  end

  @required_fields ~w(name description weight worn_on level grade)
  @optional_fields ~w(abilities global_drop)
  @rarities %{
    "common" => %{
      cost_multiplier: 1,
      color: "teal"
    },
    "uncommon" => %{
      cost_multiplier: 2,
      color: "#1eff00"
    },
    "rare" => %{
      cost_multiplier: 3,
      color: "#0070ff"
    },
    "epic" => %{
      cost_multiplier: 4,
      color: "#a335ee"
    },
    "legendary" => %{
      cost_multiplier: 5,
      color: "#ff8000"
    },
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

  def random_item_id_below_level(level) do
    count =
      __MODULE__
      |> below_level(level)
      |> global_drops
      |> select([item], count(item.id))
      |> Repo.one

    __MODULE__
    |> below_level(level)
    |> global_drops
    |> offset(fragment("floor(random()*?) LIMIT 1", ^count))
    |> select([item], item.id)
    |> Repo.one
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

  def generate_item(%{chance: chance, item_id: _item_id, level: _level} = opts) do
    if :rand.uniform(100) <= chance do
      opts
      |> Map.delete(:chance)
      |> generate_item
    end
  end

  def generate_item(%{item_id: :global, level: level}) do
    item_id = random_item_id_below_level(level)
    generate_item(%{item_id: item_id, level: level})
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

  def price_for_character(%Item{rarity: rarity} = item, %Character{level: level} = character) do
    attributes = attributes_at_level(item, level)

    trunc((attributes * @rarities[rarity].cost_multiplier))
  end

  def sell_price(%Item{rarity: rarity} = item, level) do
    attributes = attributes_at_level(item, level)

    div(trunc((attributes * @rarities[rarity].cost_multiplier)), 10)
  end

  def attributes_at_level(%Item{} = item, level) do
    base = item.strength + item.agility + item.intellect + item.willpower + item.health + item.charm

    base + (base / 10 * (level - 1))
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
