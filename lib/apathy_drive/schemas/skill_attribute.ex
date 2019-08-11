defmodule ApathyDrive.SkillAttribute do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Skill, Attribute}

  schema "skills_attributes" do
    belongs_to(:skill, Skill)
    belongs_to(:attribute, Attribute)
  end

  def changeset(model, params \\ %{}) do
    model
    |> cast(params, ~w(skill_id attribute_id)a)
  end

  def attributes(skill_id) do
    __MODULE__
    |> where([mt], mt.skill_id == ^skill_id)
    |> preload(:attribute)
    |> Repo.all()
    |> Enum.map(&String.to_existing_atom(&1.attribute.name))
  end
end
