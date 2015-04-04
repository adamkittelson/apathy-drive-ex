defmodule Commands.West do
  use ApathyDrive.Command

  def keywords, do: ["w",  "west"]

  def execute(spirit_or_monster_struct, _arguments) do
    ApathyDrive.Exit.move(spirit_or_monster_struct, "west")
  end
end
