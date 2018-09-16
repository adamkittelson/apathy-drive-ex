defmodule ApathyDrive.ItemClass do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Class, ItemClass, Item}

  schema "items_classes" do
    field(:delete, :boolean, virtual: true)

    belongs_to(:class, Class)
    belongs_to(:item, Item)
  end

  @required_fields ~w(class_id)a

  def load_classes(%Item{id: id} = item) do
    class_ids =
      __MODULE__
      |> where([ic], ic.item_id == ^id)
      |> select([c], c.class_id)
      |> Repo.all()

    Map.put(item, :required_classes, class_ids)
  end

  def changeset(%ItemClass{} = rt, attrs) do
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
