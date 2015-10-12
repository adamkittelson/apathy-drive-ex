defmodule ApathyDrive.Item do
  use ApathyDrive.Web, :model

  schema "items" do
    field :name, :string
    field :description, :string
    field :weight, :integer
    field :worn_on, :string
    field :physical_defense, :integer
    field :magical_defense, :integer
    field :level, :integer
    field :strength, :integer
    field :agility, :integer
    field :will, :integer
    field :grade, :integer

    timestamps
  end

  @required_fields ~w(name description weight worn_on physical_defense magical_defense level strength agility will grade)
  @optional_fields ~w()

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  def changeset(model, params \\ :empty) do
    model
    |> cast(params, @required_fields, @optional_fields)
  end

  def items_below_level(level) do
    query =
      from i in __MODULE__,
      where: i.level <= ^level

    Repo.all(query)
  end

  def generate_item(level) do
    level
    |> items_below_level
    |> Enum.random
    |> to_map
    |> roll_stats(level)
  end

  def to_map(nil), do: nil
  def to_map(%__MODULE__{} = item) do
    item
    |> Map.from_struct
    |> Map.take([:name, :description, :weight, :worn_on,
                 :physical_defense, :magical_defense,
                 :level, :strength, :agility, :will, :grade])
    |> Poison.encode! # dirty hack to
    |> Poison.decode! # stringify the keys
  end

  def roll_stats(nil, _rolls),   do: nil
  def roll_stats(%{} = item, 0), do: item
  def roll_stats(%{} = item, rolls) do
    if :random.uniform(10) > 7 do
      item
      |> enhance
      |> roll_stats(rolls)
    else
      roll_stats(item, rolls - 1)
    end
  end

  def enhance(%{"strength" => str, "agility" => agi, "will" => will} = item) do
    case :random.uniform(str + agi + will) do
      roll when roll > (str + agi) ->
        Map.put(item, "will", will + 1)
      roll when roll <= str ->
        Map.put(item, "strength", str + 1)
      _ ->
        Map.put(item, "agility", agi + 1)
    end
  end
end
