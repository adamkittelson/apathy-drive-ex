defmodule Commands.Experience do
  use Systems.Command

  def keywords, do: ["exp"]

  def execute(spirit, nil, _arguments) do
    send_message(spirit, "scroll", "<p><span class='dark-green'>Exp:</span> <span class='dark-cyan'>#{Components.Experience.value(spirit)}</span> <span class='dark-green'>Level:</span> <span class='dark-cyan'>#{Components.Level.value(spirit)}</span> <span class='dark-green'>Exp needed for next level:</span> <span class='dark-cyan'>0 (15641) [158%]</span></p>")
  end

  def execute(spirit, monster, _arguments) do
    send_message(spirit, "scroll", "<p><span class='dark-green'>Exp:</span> <span class='dark-cyan'>#{Components.Experience.value(monster)}</span> <span class='dark-green'>Level:</span> <span class='dark-cyan'>#{Components.Level.value(monster)}</span> <span class='dark-green'>Exp needed for next level:</span> <span class='dark-cyan'>0 (15641) [158%]</span></p>")
  end

end
