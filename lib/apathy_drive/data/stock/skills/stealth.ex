defmodule Skills.Stealth do
  use Systems.Skill

  def prereqs, do: []
  def cost,    do: 2
  def level,   do: 1

  def modifiers do
    %{
      "agility"      => 3,
      "intelligence" => 1
     }
  end

  def help do
    "This skill allows you to sneak about unnoticed.  Using the skill toggles it on and off.  Keep in mind that players and monsters with good perception skill can still see you."
  end
end
