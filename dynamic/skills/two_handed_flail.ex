defmodule Skills.TwoHandedFlail do
  use Systems.Skill

  def prereqs, do: []
  def cost,    do: 1.15
  def level,   do: 1

  def help do
    "This is skill with flail and chain-type two-handed weapons."
  end
end
