defmodule Skills.Parry do
  use Systems.Skill

  def prereqs, do: []
  def cost,    do: 1.5
  def level,   do: 1

  def help do
    "This skill allows you to parry would-be attackers."
  end
end
