defmodule Commands.North do
  use Systems.Command

  def keywords, do: ["n", "north"]

  def execute(spirit_or_monster_struct, _arguments) do
    ApathyDrive.Exit.move(spirit_or_monster_struct, "north")
  end
end
