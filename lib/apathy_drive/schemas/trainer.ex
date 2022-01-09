defmodule ApathyDrive.Trainer do
  use ApathyDriveWeb, :model
  require Logger

  alias ApathyDrive.{Class, Repo, Room, Skill, Trainer}

  schema "trainers" do
    belongs_to(:room, Room)
    belongs_to(:skill, Skill)
    belongs_to(:class, Class)
  end

  def guild_name(%Room{class_id: nil}), do: nil

  def guild_name(%Room{class_id: class_id}) do
    Repo.get(Class, class_id).name
  end

  def join_room?(%Room{class_id: class_id}) when not is_nil(class_id), do: true
  def join_room?(_room), do: false

  def trainer?(%Room{trainable_skills: list}) when is_list(list) and length(list) > 0, do: true
  def trainer?(%Room{trainable_skills: _list}), do: false

  def load(%Room{id: id} = room) do
    skills =
      Trainer
      |> Ecto.Query.where(room_id: ^id)
      |> Ecto.Query.preload([:skill])
      |> Repo.all()
      |> Enum.map(&%{skill: &1.skill, class_id: &1.class_id})

    case skills do
      [%{skill: nil, class_id: class_id}] ->
        room
        |> Map.put(:class_id, class_id)
        |> Map.put(:skills, [])

      skills ->
        Map.put(room, :trainable_skills, skills)
    end
  end
end
