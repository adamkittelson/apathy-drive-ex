defmodule ApathyDrive.ItemResistance do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Item, DamageType}

  schema "items_resistances" do
    field(:amount, :integer)

    belongs_to(:item, Item)
    belongs_to(:damage_type, DamageType)
  end

  def load_resistances(item_id) do
    __MODULE__
    |> where([mt], mt.item_id == ^item_id)
    |> preload([:damage_type])
    |> Repo.all()
    |> Enum.reduce(%{}, fn %{damage_type: damage_type, amount: amount}, resistances ->
      Map.put(resistances, "Resist" <> String.capitalize(damage_type.name), amount)
    end)
  end
end
