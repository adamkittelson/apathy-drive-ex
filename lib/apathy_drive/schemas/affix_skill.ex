defmodule ApathyDrive.AffixSkill do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Affix, Skill}

  schema "affix_skills" do
    belongs_to(:affix, Affix)
    belongs_to(:skill, Skill)

    field(:value, ApathyDrive.JSONB)
    field(:description, :string)
  end
end
