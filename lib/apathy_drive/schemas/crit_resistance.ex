defmodule ApathyDrive.CritResistance do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Crit, DamageType}

  schema "crits_resistances" do
    field(:amount, :integer)

    belongs_to(:crit, Crit)
    belongs_to(:damage_type, DamageType)
  end

  def load_resistances(crit_id) do
    __MODULE__
    |> where([mt], mt.crit_id == ^crit_id)
    |> preload([:damage_type])
    |> Repo.all()
    |> Enum.reduce(%{}, fn %{damage_type: damage_type, amount: amount}, resistances ->
      Map.put(resistances, "Resist" <> String.capitalize(damage_type.name), amount)
    end)
  end
end
