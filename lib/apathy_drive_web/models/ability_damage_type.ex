defmodule ApathyDrive.AbilityDamageType do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Ability, DamageType}

  schema "abilities_damage_types" do
    field :kind, :string
    field :potency, :integer

    belongs_to :ability, Ability
    belongs_to :damage_type, DamageType
  end

  def load_damage(ability_id) do
    __MODULE__
    |> where([mt], mt.ability_id == ^ability_id)
    |> preload([:damage_type])
    |> Repo.all
    |> Enum.reduce([], fn %{damage_type: damage_type, kind: kind, potency: potency}, damages ->
         [%{kind: kind, potency: potency, damage_type: damage_type.name, damage_type_id: damage_type.id} | damages]
       end)
  end

end
