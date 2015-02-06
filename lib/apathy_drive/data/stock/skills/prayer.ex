defmodule Skills.Prayer do
  use Systems.Skill

  def prereqs, do: []
  def cost,    do: 2.5
  def level,   do: 1

  def modifiers do
    %{
      :willpower => 3,
      :intellect => 1,
      :charm     => 1
     }
  end

  def help do
    "Prayer is a powerful form of magic practiced by clerics and priests. The spells involved include healing, protection, warding, and scrying, combined with minor illiusions and summoning."
  end
end
