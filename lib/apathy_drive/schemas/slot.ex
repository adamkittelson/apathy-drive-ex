defmodule ApathyDrive.Slot do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{LimbSetLimbSlot, Slot}

  schema "slots" do
    field :name, :string

    has_many(:limb_set_limb_slots, LimbSetLimbSlot)
  end

  @required_fields ~w(name)a

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  def changeset(%Slot{} = race, attrs \\ %{}) do
    race
    |> cast(attrs, @required_fields)
    |> validate_required(@required_fields)
    |> cast_assoc(:slots)
  end
end
