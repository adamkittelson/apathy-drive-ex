defmodule ApathyDrive.Commands.Experience do
  use ApathyDrive.Command
  alias ApathyDrive.Level

  def keywords, do: ["exp", "experience", "essence"]

  def execute(%Room{} = room, %Monster{} = monster, _arguments) do
    level     = monster.level
    exp       = trunc(monster.experience)
    remaining = trunc(max(Level.exp_to_next_level(monster), 0))
    tolevel   = Level.exp_at_level(level + 1)
    percent   = ((exp / tolevel) * 100) |> round

    Monster.send_scroll(monster, "<p><span class='dark-green'>Essence:</span> <span class='dark-cyan'>#{exp}</span> <span class='dark-green'>Level:</span> <span class='dark-cyan'>#{level}</span> <span class='dark-green'>Essence needed for next level:</span> <span class='dark-cyan'>#{remaining} (#{tolevel}) [#{percent}%]</span></p>")
    room
  end

end
