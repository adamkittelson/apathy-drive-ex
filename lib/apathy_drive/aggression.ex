defmodule ApathyDrive.Aggression do
  alias ApathyDrive.Monster

  # Don't attack other monsters
  def react(%Monster{} = monster, %Monster{}), do: monster

  # attack non-monsters if hostile
  def react(%Monster{hostile: true} = monster, %{} = intruder) do
    attack(monster, intruder)
  end

  def react(%Monster{} = monster, %{} = intruder), do: monster

  def react(%{} = mobile, %{}), do: mobile

  def attack(%{} = attacker, %{ref: ref} = intruder) do
    ApathyDrive.Commands.Attack.attack(attacker, intruder)
  end

end
