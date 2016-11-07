defmodule ApathyDrive.Commands.Experience do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Mobile, Level}

  def keywords, do: ["exp", "experience"]

  def execute(%Room{} = room, %Character{} = character, _arguments) do
    level     = character.level
    exp       = trunc(character.experience)
    remaining = trunc(max(Level.exp_to_next_level(character), 0))
    tolevel   = Level.exp_at_level(level + 1)
    percent   = ((exp / tolevel) * 100) |> round

    Mobile.send_scroll(character, "<p><span class='dark-green'>Exp:</span> <span class='dark-cyan'>#{exp}</span> <span class='dark-green'>Level:</span> <span class='dark-cyan'>#{level}</span> <span class='dark-green'>Exp needed for next level:</span> <span class='dark-cyan'>#{remaining} (#{tolevel}) [#{percent}%]</span></p>")
    room
  end

end
