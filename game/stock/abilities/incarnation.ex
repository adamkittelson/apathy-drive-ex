defmodule Abilities.Incarnation do
  use Systems.Ability

  def useable_by?(entity) do
    Components.Spirit.value(entity) == true
  end

  def help do
    """
Incarnation allows you to become mortal and become the race of your choosing.

Syntax: use incarnation at (sex) (race)
"""
  end
end
