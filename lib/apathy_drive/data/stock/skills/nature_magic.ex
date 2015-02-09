defmodule Skills.NatureMagic do
  use Systems.Skill

  def prereqs, do: []
  def cost,    do: 2.3
  def level,   do: 1

  def modifiers do
    %{
      "intelligence" => 2,
      "health"       => 1
     }
  end


  def help do
    "Allows casting spells relating to nature including elemntal attacks, protection, and healing."
  end
end
