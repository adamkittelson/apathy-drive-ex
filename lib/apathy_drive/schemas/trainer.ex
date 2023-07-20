defmodule ApathyDrive.Trainer do
  use ApathyDriveWeb, :model
  require Logger

  alias ApathyDrive.{Character, Class, Mobile, Repo, Room, Skill, Trainer}

  schema "trainers" do
    belongs_to(:room, Room)
    belongs_to(:skill, Skill)
    belongs_to(:class, Class)
  end

  def dev_cost(%Character{} = character, %Skill{type: "skill"} = skill) do
    times = Skill.module(skill.name).current_level_times_trained(character)

    times + 1
  end

  def dev_cost(%Character{} = character, %Skill{type: "ability"} = skill) do
    times = Skill.module(skill.name).current_level_times_trained(character)
    power_level = Skill.module(skill.name).skill_level(character) + 1

    power_level =
      if power_level > Skill.module(skill.name).max_skill_level(character) do
        0
      else
        power_level
      end

    IO.puts(
      "times: #{times}, power level: #{power_level}, cost: #{skill.dev_cost}/#{skill.fast_dev_cost}"
    )

    power_level * times
  end

  def guild_name(%Room{class_id: nil}), do: nil

  def guild_name(%Room{class_id: class_id}) do
    Repo.get(Class, class_id).name
  end

  def join_room?(%Room{class_id: class_id}) when not is_nil(class_id),
    do: true

  def join_room?(_room), do: false

  def trainer?(%Room{trainable_skills: list}) when is_list(list) and length(list) > 0, do: true
  def trainer?(%Room{trainable_skills: _list}), do: false

  def skill_trainer?(%Room{trainable_skills: list}) when is_list(list) and length(list) > 0 do
    Enum.any?(list, fn %{skill: skill} ->
      skill.type == "skill"
    end)
  end

  def skill_trainer?(%Room{trainable_skills: _list}), do: false

  def ability_trainer?(%Room{trainable_skills: list}) when is_list(list) and length(list) > 0 do
    Enum.any?(list, fn %{skill: skill} ->
      skill.type == "ability"
    end)
  end

  def ability_trainer?(%Room{trainable_skills: _list}), do: false

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

      [%{class_id: class_id} | _rest] = skills ->
        room
        |> Map.put(:trainable_skills, skills)
        |> Map.put(:class_id, class_id)

      skills ->
        Map.put(room, :trainable_skills, skills)
    end
  end

  def training_cost(%Character{level: level} = character) do
    charm = Mobile.attribute_at_level(character, :charm, character.level)

    cost_multiplier = trunc(3.5 + 2.43 * (level - 2))

    next_level = level + 1
    charm_mod = 1 - (trunc(charm / 5.0) - 10) / 100
    trunc(next_level * 5 * (cost_multiplier + 1) * 10 * charm_mod)
  end
end
