defmodule ApathyDrive.Limb do
  use ApathyDriveWeb, :model
  alias ApathyDrive.Limb

  schema "limbs" do
    field(:type, :string)
  end

  @required_fields ~w(name)a

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  def changeset(%Limb{} = race, attrs \\ %{}) do
    race
    |> cast(attrs, @required_fields)
    |> validate_required(@required_fields)
    |> cast_assoc(:limb_sets)
  end
end
