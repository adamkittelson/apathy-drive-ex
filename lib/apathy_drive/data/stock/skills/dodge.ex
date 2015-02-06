defmodule Skills.Dodge do
  use Systems.Skill

  def prereqs, do: []
  def cost,    do: 2.0
  def level,   do: 1

  def modifiers do
    %{
      :agility => 3,
      :health  => 1,
      :charm   => 1
     }
  end

  def help do
    "This skill allows you to dodge attacks."
  end
end
