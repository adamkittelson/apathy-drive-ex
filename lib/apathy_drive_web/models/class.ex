defmodule ApathyDrive.Class do
  use ApathyDrive.Web, :model

  schema "classes" do
    field :name, :string
    field :description, :string
    field :armour, :string
    field :weapon_hands, :string
    field :weapon_type, :string
    field :abilities, ApathyDrive.JSONB, default: []

    has_many :characters, ApathyDrive.Character

    timestamps()
  end

  @required_fields ~w(name description armour weapon_hands weapon_type abilities)a
  @optional_fields ~w()

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  def changeset(model, params \\ %{}) do
    model
    |> cast(params, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
    |> validate_inclusion(:weapon_hands, weapon_hands())
    |> validate_inclusion(:weapon_type, weapon_types())
  end

  def weapon_hands do
    ["Any", "One Handed", "Two Handed"]
  end

  def weapon_types do
    ["Any", "Blunt", "Bladed", "Basic"]
  end

  def ids do
    Repo.all(__MODULE__, select: [:id])
    |> Enum.map(&(Map.get(&1, :id)))
  end

  def names do
    Repo.all(__MODULE__, select: [:name])
    |> Enum.map(&(Map.get(&1, :name)))
  end

  def select do
    Repo.all(__MODULE__, select: [:id, :name])
    |> Enum.map(&({&1.name, &1.id}))
  end

  def start_room(id) do
    Repo.get(__MODULE__, id).start_room_id
  end

  def datalist do
    __MODULE__
    |> Repo.all
    |> Enum.map(fn(class) ->
         "#{class.name} - #{class.id}"
       end)
  end

end
