defmodule ApathyDrive.ItemTrait do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Item, ItemTrait, Trait}

  schema "items_traits" do
    field(:value, ApathyDrive.JSONB)
    field(:delete, :boolean, virtual: true)

    belongs_to(:item, Item)
    belongs_to(:trait, Trait)
  end

  @required_fields ~w(trait_id value)a

  def load_traits(item_id) do
    __MODULE__
    |> where([mt], mt.item_id == ^item_id)
    |> preload([:trait])
    |> Repo.all()
    |> Enum.reduce(%{}, fn %{trait: trait, value: value}, abilities ->
      abilities = Map.put_new(abilities, trait.name, [])
      Map.put(abilities, trait.name, [value | abilities[trait.name]])
    end)
  end

  def changeset(%ItemTrait{} = rt, attrs) do
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
