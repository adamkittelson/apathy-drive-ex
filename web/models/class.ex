defmodule ApathyDrive.Class do
  use ApathyDrive.Web, :model

  schema "classes" do
    field :name, :string
    field :alignment, :string
    field :start_room_id, :integer
    field :abilities, ApathyDrive.JSONB, default: []
    field :unities, ApathyDrive.JSONB, default: []

    has_many :spirits, Spirit

    timestamps
  end

  @required_fields ~w(name alignment start_room_id unities abilities)
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
