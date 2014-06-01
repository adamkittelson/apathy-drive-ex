defmodule Skills.DualAttack do
  use Systems.Skill

  def prereqs, do: []
  def cost,    do: 4.2
  def level,   do: 1

  def help do
    "This skill helps offset the penalty which the player incurs while attacking with more than one weapon."
  end
end
