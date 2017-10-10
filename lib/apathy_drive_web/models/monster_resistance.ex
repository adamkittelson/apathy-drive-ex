defmodule ApathyDrive.MonsterResistance do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Monster, DamageType}

  schema "monsters_resistances" do
    field :amount, :integer

    belongs_to :monster, Monster
    belongs_to :damage_type, DamageType
  end

  def load_resistances(monster_id) do
    __MODULE__
    |> where([mt], mt.monster_id == ^monster_id)
    |> preload([:damage_type])
    |> Repo.all
    |> Enum.reduce(%{}, fn %{damage_type: damage_type, amount: amount}, resistances ->
         Map.put(resistances, "Resist" <> String.capitalize(damage_type.name), amount)
       end)
  end

end
