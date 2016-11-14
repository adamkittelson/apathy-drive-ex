defmodule ApathyDrive.Aggression do
  alias ApathyDrive.Monster

  # Don't attack other monsters
  def react(%Monster{} = monster, %Monster{}), do: monster

  # attack non-monsters if hostile
  def react(%Monster{hostile: true} = monster, %{} = intruder) do
    attack(monster, intruder)
  end

  def attack(%Monster{} = monster, %Monster{ref: ref}) do
    update_in(monster.hate[ref], &((&1 || 0) + 1))
  end

end
