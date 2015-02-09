defmodule Skills.Block do
  use Systems.Skill

  def prereqs, do: []
  def cost,    do: 1.8
  def level,   do: 1

  def modifiers do
    %{
      "strength"  => 2,
      "health"    => 2
     }
  end

  def help do
    "This skill allows you to block attacks with your shield."
  end
end
