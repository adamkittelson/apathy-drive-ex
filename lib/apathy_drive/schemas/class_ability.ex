defmodule ApathyDrive.ClassAbility do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Ability, Class}

  schema "classes_abilities" do
    field(:level, :integer)
    field(:required_strength, :integer)
    field(:required_agility, :integer)
    field(:required_intellect, :integer)
    field(:required_willpower, :integer)
    field(:required_health, :integer)
    field(:required_charm, :integer)

    belongs_to(:ability, Ability)
    belongs_to(:class, Class)
  end

  def changeset(model, params \\ %{}) do
    model
    |> cast(params, ~w(damage_type_id kind potency)a)
  end

  def abilities_at_level(class_id, level) do
    ApathyDrive.ClassAbility
    |> Ecto.Query.where([ss], ss.class_id == ^class_id and ss.level <= ^level)
    |> Ecto.Query.preload([:ability])
    |> Repo.all()
  end

  def load_damage(ability_id) do
    __MODULE__
    |> where([mt], mt.ability_id == ^ability_id)
    |> preload([:damage_type])
    |> Repo.all()
    |> Enum.reduce([], fn %{damage_type: damage_type, kind: kind, potency: potency}, damages ->
      [
        %{
          kind: kind,
          potency: potency,
          damage_type: damage_type.name,
          damage_type_id: damage_type.id
        }
        | damages
      ]
    end)
  end
end
