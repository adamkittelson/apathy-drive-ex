defmodule ApathyDrive.ClassSkill do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Class, Skill}

  schema "classes_skills" do
    belongs_to(:class, Class)
    belongs_to(:skill, Skill)
  end
end
