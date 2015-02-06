defmodule Skills.Armour do
  use Systems.Skill

  def prereqs, do: []
  def cost,    do: 1.0
  def level,   do: 1

  def modifiers do
    %{
      :strength  => 1,
      :health    => 1
     }
  end


  def help do
    "This is skill with one-handed bladed weapons."
  end
end
