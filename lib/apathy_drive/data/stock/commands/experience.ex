defmodule Commands.Experience do
  use Systems.Command

  def keywords, do: ["exp"]

  def execute(spirit, nil, _arguments) do
    execute(spirit)
  end

  def execute(spirit, monster, _arguments) do
    execute(monster)
  end

  def execute(entity) do
    level     = Components.Level.value(entity)
    exp       = Components.Experience.value(entity)
    remaining = max(Systems.Level.exp_to_next_level(entity), 0)
    tolevel   = Systems.Level.exp_at_level(level + 1)
    percent   = ((exp / tolevel) * 100) |> round

    send_message(entity, "scroll", "<p><span class='dark-green'>Exp:</span> <span class='dark-cyan'>#{exp}</span> <span class='dark-green'>Level:</span> <span class='dark-cyan'>#{level}</span> <span class='dark-green'>Exp needed for next level:</span> <span class='dark-cyan'>#{remaining} (#{tolevel}) [#{percent}%]</span></p>")
  end

end
