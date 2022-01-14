defmodule ApathyDrive.CharacterSkill do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Character, Class, Skill}

  schema "characters_skills" do
    field(:auto, :boolean)
    field(:level, :integer)
    field(:current_level_times_trained, :integer)
    field(:devs_spent, :integer)

    belongs_to(:character, Character)
    belongs_to(:skill, Skill)
    belongs_to(:class, Class)
  end
end
