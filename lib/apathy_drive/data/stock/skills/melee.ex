defmodule Skills.Melee do
  use Systems.Skill

  def prereqs, do: []
  def cost,    do: 2
  def level,   do: 1

  def modifiers do
    %{
      :strength  => 2,
      :agility   => 2,
      :health    => 2,
      :charm     => 1
     }
  end

  def help do
    "This is a skill of unarmed hand-to-hand combat.  High skill gives you better damage and sometimes more attacks."
  end
end
