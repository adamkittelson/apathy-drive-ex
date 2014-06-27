defmodule Skills.NotFound do
  use Systems.Skill

  def prereqs, do: []
  def cost,    do: 1.0
  def level,   do: 1
  def modifiers, do: %{}
  def help, do: nil
end
