defmodule Commands.South do
  use Systems.Command

  def keywords, do: ["s",  "south"]

  def execute(spirit_or_monster_struct, _arguments) do
    ApathyDrive.Exit.move(spirit_or_monster_struct, "south")
  end
end
