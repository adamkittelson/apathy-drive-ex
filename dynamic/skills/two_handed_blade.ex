defmodule Skills.TwoHandedBlade do
  use Systems.Skill

  def prereqs, do: []
  def cost,    do: 1.3
  def level,   do: 1

  def help do
    "This is skill with two-handed bladed weapons."
  end
end
