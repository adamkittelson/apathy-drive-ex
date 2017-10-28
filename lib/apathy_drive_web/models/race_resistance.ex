defmodule ApathyDrive.RaceResistance do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Race, DamageType}

  schema "races_resistances" do
    field :amount, :integer

    belongs_to :race, Race
    belongs_to :damage_type, DamageType
  end

  def load_resistances(race_id) do
    __MODULE__
    |> where([mt], mt.race_id == ^race_id)
    |> preload([:damage_type])
    |> Repo.all
    |> Enum.reduce(%{}, fn %{damage_type: damage_type, amount: amount}, resistances ->
         Map.put(resistances, "Resist" <> String.capitalize(damage_type.name), amount)
       end)
  end

end
