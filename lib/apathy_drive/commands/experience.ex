defmodule Commands.Experience do
  use ApathyDrive.Command

  def keywords, do: ["exp"]

  def execute(mobile, _arguments) do
    Mobile.display_experience(mobile)
  end

  def message(entity) do
    level     = entity.level
    exp       = entity.experience
    remaining = max(Systems.Level.exp_to_next_level(entity), 0)
    tolevel   = Systems.Level.exp_at_level(level + 1)
    percent   = ((exp / tolevel) * 100) |> round

    "<p><span class='dark-green'>Exp:</span> <span class='dark-cyan'>#{exp}</span> <span class='dark-green'>Level:</span> <span class='dark-cyan'>#{level}</span> <span class='dark-green'>Exp needed for next level:</span> <span class='dark-cyan'>#{remaining} (#{tolevel}) [#{percent}%]</span></p>"
  end

end
