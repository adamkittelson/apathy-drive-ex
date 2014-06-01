defmodule Skills.Axe do
  use Systems.Skill

  def prereqs, do: []
  def cost,    do: 1.0
  def level,   do: 1

  def help do
    "This is skill with hand axes and other one-handed axes."
  end
end
