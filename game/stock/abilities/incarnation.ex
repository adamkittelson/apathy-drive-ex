defmodule Abilities.Incarnation do
  use Systems.Ability

  def useable_by?(entity) do
    Components.Spirit.value(entity) == true
  end

  def help do
    """
Incarnation allows you to become mortal as the race of your choosing.
More powerful races require a larger investment of power, leaving you less to spend on training in your mortal life.

Syntax: `use incarnation at (sex) (race)`

`help races` to get a list of available races
"""
  end
end
