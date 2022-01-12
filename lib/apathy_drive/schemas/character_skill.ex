defmodule ApathyDrive.CharacterSkill do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Character, Skill}

  schema "characters_skills" do
    field(:auto, :boolean)
    field(:level, :integer)
    field(:current_level_times_trained, :integer)

    belongs_to(:character, Character)
    belongs_to(:skill, Skill)
  end
end
