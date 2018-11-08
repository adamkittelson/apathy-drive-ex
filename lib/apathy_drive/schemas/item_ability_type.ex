defmodule ApathyDrive.ItemAbilityType do
  use ApathyDriveWeb, :model
  alias ApathyDrive.ItemAbilityType

  schema "item_ability_types" do
    field(:name, :string)

    field(:delete, :boolean, virtual: true)

    has_many(:item_abilities, ApathyDrive.ItemAbility)
  end

  @required_fields ~w(name)a

  def changeset(%ItemAbilityType{} = rt, attrs) do
    rt
    |> cast(attrs, [:delete | @required_fields])
    |> validate_required(@required_fields)
    |> mark_for_deletion()
  end

  defp mark_for_deletion(changeset) do
    # If delete was set and it is true, let's change the action
    if get_change(changeset, :delete) do
      %{changeset | action: :delete}
    else
      changeset
    end
  end
end
