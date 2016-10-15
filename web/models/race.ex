defmodule ApathyDrive.Race do
  use ApathyDrive.Web, :model

  schema "races" do
    field :name, :string
    field :description, :string
    field :strength, :integer
    field :agility, :integer
    field :intellect, :integer
    field :willpower, :integer
    field :health, :integer
    field :charm, :integer

    timestamps
  end

  @required_fields ~w(name description strength agility intellect willpower health charm)
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

end
