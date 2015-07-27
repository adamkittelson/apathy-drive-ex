defmodule ApathyDrive.Class do
  use ApathyDrive.Web, :model
  import Ecto.Query

  schema "classes" do
    field :name, :string
    field :description, :string
    field :properties, ApathyDrive.JSONB

    timestamps
  end

  @required_fields ~w(name description properties)
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

  def list_for_select do
    ApathyDrive.Class
    |> select([r], {r.name, r.id})
    |> Repo.all
  end
end
