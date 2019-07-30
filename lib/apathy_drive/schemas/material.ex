defmodule ApathyDrive.Material do
  use ApathyDriveWeb, :model

  require Logger
  require Ecto.Query

  schema "materials" do
    field(:name, :string)
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
end
