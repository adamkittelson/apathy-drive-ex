defmodule ApathyDrive.Race do
  use ApathyDriveWeb, :model

  schema "races" do
    field(:name, :string)
    field(:description, :string)
    field(:strength, :integer)
    field(:agility, :integer)
    field(:intellect, :integer)
    field(:willpower, :integer)
    field(:health, :integer)
    field(:charm, :integer)
    field(:abilities, ApathyDrive.JSONB)

    has_many(:races_traits, ApathyDrive.RaceTrait)
    has_many(:traits, through: [:races_traits, :trait])

    timestamps()
  end

  @required_fields ~w(name description strength agility intellect willpower health charm abilities)
  @optional_fields ~w()

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  def changeset(model, params \\ %{}) do
    model
    |> cast(params, @required_fields, @optional_fields)
  end

  def ids do
    Repo.all(__MODULE__, select: [:id])
    |> Enum.map(&Map.get(&1, :id))
  end

  def select do
    Repo.all(__MODULE__, select: [:id, :name])
    |> Enum.map(&{&1.name, &1.id})
  end

  def all do
    Repo.all(__MODULE__, select: [:id, :name, :description])
  end

  def name(race_id) do
    __MODULE__
    |> Ecto.Query.where(id: ^race_id)
    |> Ecto.Query.select([:name])
    |> Repo.one()
    |> Map.get(:name)
  end
end
