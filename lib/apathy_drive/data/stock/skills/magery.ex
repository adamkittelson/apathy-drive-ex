defmodule Skills.Magery do
  use Systems.Skill

  def prereqs, do: []
  def cost,    do: 2.3
  def level,   do: 1

  def modifiers do
    %{
      "intelligence" => 3,
      "agility"      => 1
     }
  end


  def help do
    "This is a general magic skill which allows casting of spells dealing with protection, summoning, magical attack, and some miscellaneous spells."
  end
end
