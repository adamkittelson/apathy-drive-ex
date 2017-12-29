defmodule ApathyDrive.Commands.Experience do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Match, Mobile, Level}

  def keywords, do: ["exp", "experience"]

  def execute(%Room{} = room, %Character{} = character, []) do
    show_character_experience(character)
    room
  end

  def execute(%Room{} = room, %Character{} = character, args) do
    skill =
      character.skills
      |> Map.values
      |> Match.one(:keyword_starts_with, Enum.join(args, " "))

    show_skill_experience(character, skill)
    room
  end

  defp show_character_experience(character) do
    level     = character.level
    exp       = trunc(character.experience)
    remaining = trunc(max(Level.exp_to_next_level(character.level, character.experience), 0))
    tolevel   = Level.exp_at_level(level + 1)
    percent   = ((exp / tolevel) * 100) |> round

    Mobile.send_scroll(character, "<p><span class='dark-green'>Name:</span> <span class='dark-cyan'>#{character.name}</span> <span class='dark-green'>Exp:</span> <span class='dark-cyan'>#{exp}</span> <span class='dark-green'>Level:</span> <span class='dark-cyan'>#{level}</span> <span class='dark-green'>Exp needed for next level:</span> <span class='dark-cyan'>#{remaining} (#{tolevel}) [#{percent}%]</span></p>")
  end

  defp show_skill_experience(character, skill) do
    level     = skill.level
    exp       = trunc(skill.experience)
    remaining = trunc(max(Level.exp_to_next_skill_level(skill.level, skill.experience, skill.training_cost_multiplier), 0))
    tolevel   = Level.exp_at_level(level + 1)
    percent   = ((exp / tolevel) * 100) |> round

    Mobile.send_scroll(character, "<p><span class='dark-green'>Skill:</span> <span class='dark-cyan'>#{skill.name}</span> <span class='dark-green'>Exp:</span> <span class='dark-cyan'>#{exp}</span> <span class='dark-green'>Level:</span> <span class='dark-cyan'>#{level}</span> <span class='dark-green'>Exp needed for next level:</span> <span class='dark-cyan'>#{remaining} (#{tolevel}) [#{percent}%]</span></p>")
  end

end
