defmodule Skills.Blunt do
  use Systems.Skill

  def prereqs, do: []
  def cost,    do: 1.0
  def level,   do: 1

  def help do
    "This is skill with one-handed blunt weapons."
  end
end
