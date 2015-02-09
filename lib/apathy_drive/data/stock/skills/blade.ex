defmodule Skills.Blade do
  use Systems.Skill

  def prereqs, do: []
  def cost,    do: 0.8
  def level,   do: 1

  def modifiers do
    %{
      "strength"  => 2,
      "agility"   => 2,
      "health"    => 1
     }
  end


  def help do
    "This is skill with one-handed bladed weapons."
  end
end
