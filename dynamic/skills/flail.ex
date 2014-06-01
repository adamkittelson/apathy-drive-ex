defmodule Skills.Flail do
  use Systems.Skill

  def prereqs, do: []
  def cost,    do: 1.2
  def level,   do: 1

  def help do
    "This is skill with flail and chain-type one-handed weapons."
  end
end
