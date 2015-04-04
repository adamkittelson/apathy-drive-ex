defmodule Commands.East do
  use ApathyDrive.Command

  def keywords, do: ["e",  "east"]

  def execute(spirit_or_monster_struct, _arguments) do
    ApathyDrive.Exit.move(spirit_or_monster_struct, "east")
  end
end
