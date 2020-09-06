defmodule ApathyDrive.MonsterItem do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Monster, MonsterItem, Item}

  schema "monsters_items" do
    field(:chance, :integer)
    field(:delete, :boolean, virtual: true)

    belongs_to(:monster, Monster)
    belongs_to(:item, Item)
  end

  @required_fields ~w(class_id)a

  def load_drops(%Monster{id: id} = monster) do
    drops =
      __MODULE__
      |> where([ic], ic.monster_id == ^id)
      |> select([:item_id, :chance, :id])
      |> Repo.all()

    Map.put(monster, :drops, drops)
  end

  def changeset(%MonsterItem{} = rt, attrs) do
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
