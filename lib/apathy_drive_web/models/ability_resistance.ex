defmodule ApathyDrive.AbilityResistance do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Ability, DamageType}

  schema "abilities_resistances" do
    field :amount, :integer

    belongs_to :ability, Ability
    belongs_to :damage_type, DamageType
  end

  def load_resistances(ability_id) do
    __MODULE__
    |> where([mt], mt.ability_id == ^ability_id)
    |> preload([:damage_type])
    |> Repo.all
    |> Enum.reduce(%{}, fn %{damage_type: damage_type, amount: amount}, resistances ->
         Map.put(resistances, "Resist" <> String.capitalize(damage_type.name), amount)
       end)
  end

end
