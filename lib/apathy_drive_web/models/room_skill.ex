defmodule ApathyDrive.RoomSkill do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Room, Skill}

  schema "rooms_skills" do
    belongs_to :room, Room
    belongs_to :skill, Skill
  end

end
