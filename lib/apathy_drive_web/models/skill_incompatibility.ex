defmodule ApathyDrive.SkillIncompatibility do
  use ApathyDrive.Web, :model
  alias ApathyDrive.Skill

  schema "skills_incompatibilities" do
    belongs_to :skill, Skill
    belongs_to :incompatible_skill, Skill
  end

end
