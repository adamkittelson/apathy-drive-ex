defmodule ApathyDrive.SkillAbility do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Ability, Skill}

  schema "skills_abilities" do
    belongs_to(:skill, Skill)
    belongs_to(:ability, Ability)
    field(:level, :integer)
  end

  def abilities_at_level(skill_id, level) do
    ApathyDrive.SkillAbility
    |> Ecto.Query.where(
      [ss],
      ss.skill_id == ^skill_id and ss.level <= ^level
    )
    |> Ecto.Query.preload([:ability])
    |> Repo.all()
    |> Enum.map(&put_in(&1, [Access.key!(:ability), Access.key!(:level)], &1.level))
  end
end
