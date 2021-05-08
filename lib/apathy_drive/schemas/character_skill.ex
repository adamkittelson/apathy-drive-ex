defmodule ApathyDrive.CharacterSkill do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Character, Skill}

  schema "characters_skills" do
    field(:level, :integer)
    field(:auto, :boolean)

    belongs_to(:character, Character)
    belongs_to(:skill, Skill)
  end
end
