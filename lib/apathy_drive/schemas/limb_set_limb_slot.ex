defmodule ApathyDrive.LimbSetLimbSlot do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{LimbSetLimb, LimbSetLimbSlot, Slot}

  schema "limb_set_limb_slots" do
    belongs_to(:limb_set_limb, LimbSetLimb)
    belongs_to(:slot, Slot)
  end

  @required_fields ~w()a

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  def changeset(%LimbSetLimbSlot{} = race, attrs \\ %{}) do
    race
    |> cast(attrs, @required_fields)
    |> validate_required(@required_fields)
    |> cast_assoc(:limb_set_limb_slots)
  end
end
