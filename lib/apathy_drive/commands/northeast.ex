defmodule Commands.Northeast do
  use ApathyDrive.Command

  def keywords, do: ["ne",  "northeast"]

  def execute(spirit_or_monster_struct, _arguments) do
    ApathyDrive.Exit.move(spirit_or_monster_struct, "northeast")
  end
end
