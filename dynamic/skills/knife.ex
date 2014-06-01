defmodule Skills.Knife do
  use Systems.Skill

  def prereqs, do: []
  def cost,    do: 0.8
  def level,   do: 1

  def help do
    "This is skill with knives and other stabbing weapons."
  end
end
