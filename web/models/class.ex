defmodule ApathyDrive.Class do
  use ApathyDrive.Web, :model

  schema "classes" do
    field :name, :string
    field :alignment, :string
    field :start_room_id, :integer
    field :strength, :integer
    field :strength_per_level, :integer
    field :agility, :integer
    field :agility_per_level, :integer
    field :will, :integer
    field :will_per_level, :integer
    field :abilities, ApathyDrive.JSONB

    has_many :spirits, Spirit

    timestamps
  end

  @required_fields ~w(name alignment strength strength_per_level agility agility_per_level will will_per_level abilities start_room_id)
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

  def ids do
    Repo.all(__MODULE__, select: [:id])
    |> Enum.map(&(Map.get(&1, :id)))
  end

  def select do
    Repo.all(__MODULE__, select: [:id, :name])
    |> Enum.map(&({&1.name, &1.id}))
  end

  def start_room(id) do
    Repo.get(__MODULE__, id).start_room_id
  end

end
