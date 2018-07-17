defmodule ApathyDrive.RoomSkill do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Room, Skill}

  schema "rooms_skills" do
    belongs_to(:room, Room)
    belongs_to(:skill, Skill)
  end
end
