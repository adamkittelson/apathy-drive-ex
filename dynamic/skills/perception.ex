defmodule Skills.Perception do
  use Systems.Skill

  def prereqs, do: []
  def cost,    do: 1.3
  def level,   do: 1

  def help do
    "This skill is used to spot hidden objects, as the name suggests.  The object can be anything from a hidden treasure to a hiding thief!"
  end
end
