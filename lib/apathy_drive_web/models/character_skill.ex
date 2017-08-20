defmodule ApathyDrive.CharacterSkill do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Character, Skill}

  schema "characters_skills" do
    field :experience, :integer

    belongs_to :character, Character
    belongs_to :skill, Skill
  end

end
