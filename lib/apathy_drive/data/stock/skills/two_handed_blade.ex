defmodule Skills.TwoHandedBlade do
  use Systems.Skill

  def prereqs, do: []
  def cost,    do: 1.3
  def level,   do: 1

  def modifiers do
    %{
      "strength"  => 2,
      "agility"   => 1,
      "health"    => 1
     }
  end


  def help do
    "This is skill with two-handed bladed weapons."
  end
end
