defmodule ApathyDrive.Trainer do
  use ApathyDriveWeb, :model
  require Logger

  alias ApathyDrive.{Room, Skill, Trainer}

  schema "trainers" do
    belongs_to(:room, Room)
    belongs_to(:skill, Skill)
  end

  def trainer?(%Room{trainable_skills: []}), do: false
  def trainer?(%Room{}), do: true

  def load(%Room{id: id} = room) do
    skills =
      Trainer
      |> Ecto.Query.where(room_id: ^id)
      |> Ecto.Query.preload([:skill])
      |> Repo.all()
      |> Enum.map(& &1.skill)

    Map.put(room, :trainable_skills, skills)
  end
end
