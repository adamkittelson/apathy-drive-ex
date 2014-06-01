defmodule Skills.Dodge do
  use Systems.Skill

  def prereqs, do: []
  def cost,    do: 3.5
  def level,   do: 1

  def help do
    "This skill allows you to dodge attacks."
  end
end
