defmodule Commands.Down do
  use ApathyDrive.Command

  def keywords, do: ["d", "down"]

  def execute(spirit_or_monster_struct, _arguments) do
    ApathyDrive.Exit.move(spirit_or_monster_struct, "down")
  end
end
