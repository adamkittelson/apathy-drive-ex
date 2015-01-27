defmodule Commands.East do
  use Systems.Command

  def keywords, do: ["e",  "east"]

  def execute(spirit_or_monster_struct, _arguments) do
    ApathyDrive.Exit.move(spirit_or_monster_struct, "east")
  end
end
