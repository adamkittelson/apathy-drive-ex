defmodule ApathyDrive.DamageType do
  use ApathyDrive.Web, :model

  schema "damage_types" do
    field :name, :string

    has_many :abilities_damage_types, ApathyDrive.AbilityDamageType
    has_many :abilities, through: [:abilities_damage_types, :ability]
  end

  def data_for_admin_index do
    __MODULE__
    |> select([dt], map(dt, [:id, :name]))
  end

  def names do
    __MODULE__
    |> Ecto.Query.select([:name])
    |> Repo.all
    |> Enum.map(& &1.name)
    |> Enum.sort
  end

end
