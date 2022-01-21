defmodule ApathyDrive.AbilityDamageType do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Ability, DamageType}

  schema "abilities_damage_types" do
    field(:kind, :string)
    field(:potency, :integer)
    field(:min, :integer)
    field(:max, :integer)

    belongs_to(:ability, Ability)
    belongs_to(:damage_type, DamageType)
  end

  def changeset(model, params \\ %{}) do
    model
    |> cast(params, ~w(damage_type_id kind potency)a)
  end

  def load_damage(nil), do: []

  def load_damage(ability_id) do
    __MODULE__
    |> where([mt], mt.ability_id == ^ability_id)
    |> preload([:damage_type])
    |> Repo.all()
    |> Enum.reduce([], fn %{damage_type: damage_type, min: min, max: max}, damages ->
      [
        %{
          min: min,
          max: max,
          damage_type: damage_type.name
        }
        | damages
      ]
    end)
  end
end
