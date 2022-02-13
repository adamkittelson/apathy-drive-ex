defmodule ApathyDrive.Trainer do
  use ApathyDriveWeb, :model
  require Logger

  alias ApathyDrive.{Character, Class, Mobile, Repo, Room, Skill, Trainer}

  schema "trainers" do
    field :cost_modifier, :float

    belongs_to(:room, Room)
    belongs_to(:skill, Skill)
    belongs_to(:class, Class)
  end

  def dev_cost(%Character{} = character, %Skill{type: "skill"} = skill, multiplier) do
    times = Skill.module(skill.name).current_level_times_trained(character)

    if times > 0 do
      multiplier * times * skill.fast_dev_cost
    else
      multiplier * skill.dev_cost
    end
    |> trunc()
  end

  def dev_cost(%Character{} = character, %Skill{type: "ability"} = skill, multiplier) do
    times = Skill.module(skill.name).current_level_times_trained(character)
    power_level = Skill.module(skill.name).skill_level(character) + 1

    power_level =
      if power_level > Skill.module(skill.name).max_skill_level(character) do
        0
      else
        power_level
      end

    IO.puts(
      "times: #{times}, power level: #{power_level}, mult: #{multiplier}, cost: #{skill.dev_cost}/#{skill.fast_dev_cost}"
    )

    if times > 0 do
      power_level * multiplier * times * skill.fast_dev_cost
    else
      power_level * multiplier * skill.dev_cost
    end
    |> trunc()
  end

  def guild_name(%Room{class_id: nil}), do: nil

  def guild_name(%Room{class_id: class_id}) do
    Repo.get(Class, class_id).name
  end

  def join_room?(%Room{class_id: class_id, trainable_skills: nil}) when not is_nil(class_id),
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
      |> Enum.map(&%{skill: &1.skill, class_id: &1.class_id, cost_modifier: &1.cost_modifier})

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
