defmodule ApathyDrive.Player do
  use ApathyDrive.Web, :model

  schema "players" do
    field :external_id, :string
    field :admin, :boolean, default: false

    timestamps
  end

  @required_fields ~w(external_id admin)
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
end
