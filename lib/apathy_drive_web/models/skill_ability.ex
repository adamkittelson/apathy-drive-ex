defmodule ApathyDrive.SkillAbility do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Ability, Skill}

  schema "skills_abilities" do
    belongs_to :skill, Skill
    belongs_to :ability, Ability
    field :level, :integer
  end

end
