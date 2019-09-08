defmodule ApathyDrive.LimbSetLimb do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{LimbSet, LimbSetLimb, Limb}

  schema "limb_set_limbs" do
    field(:location, :string)

    belongs_to(:limb_set, LimbSet)
    belongs_to(:limb, Limb)
  end

  @required_fields ~w(name)a

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  def changeset(%LimbSet{} = race, attrs \\ %{}) do
    race
    |> cast(attrs, @required_fields)
    |> validate_required(@required_fields)
    |> cast_assoc(:limb_sets)
  end
end
