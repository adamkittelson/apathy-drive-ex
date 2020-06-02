defmodule ApathyDrive.SkillAbility do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Ability, AbilityAttribute, Skill}

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
    |> Enum.map(fn skill_ability ->
      skill_ability = put_in(skill_ability.ability.level, skill_ability.level)
      skill_ability = put_in(skill_ability.ability.skill_id, skill_id)

      put_in(
        skill_ability.ability.attributes,
        AbilityAttribute.load_attributes(skill_ability.ability.id)
      )
    end)
  end
end
