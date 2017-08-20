defmodule ApathyDrive.Commands.Train do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Level, Skill, Mobile}

  def keywords, do: ["list"]

  def execute(%Room{} = room, %Character{} = character, skill_name) do
    if Room.trainer?(room) do
      train_skill(room, character, skill_name)
    else
      Mobile.send_scroll(character, "<p><span class='red'>You cannot TRAIN a skill if you are not at a trainer!</span></p>")
      room
    end
  end

  def train_skill(%Room{} = room, character, %Skill{} = skill) do
    exp = Map.get(character.skills, skill.name, 0)
    level = Level.skill_level_at_exp(exp, skill.training_cost_multiplier)
    cost = Level.exp_to_next_skill_level(level, exp, skill.training_cost_multiplier)

    if cost > character.experience do
      Mobile.send_scroll(character, "<p>You don't have enough experience to train #{skill.name}.</p>")
    else
      Room.update_mobile(room, character.ref, fn character ->
        character
        |> Character.add_experience(-cost)
        |> Character.train_skill(skill, cost)
      end)
    end
  end

  def train_skill(%Room{skills: skills} = room, character, skill_args) do
    skill_name = Enum.join(skill_args, " ")
    skill = Skill.match_by_name(skill_name)

    cond do
      is_nil(skill) ->
        Mobile.send_scroll(character, "<p>You cannot train #{skill_name} here.</p>")
        room
      !(Enum.map(skills, &(&1.name)) |> Enum.member?(skill.name)) ->
        Mobile.send_scroll(character, "<p>You cannot train #{skill.name} here.</p>")
        room
      true ->
        train_skill(room, character, skill)
    end
  end
end
