defmodule Commands.Southwest do
  use ApathyDrive.Command

  def keywords, do: ["sw", "southwest"]

  def execute(spirit_or_monster_struct, _arguments) do
    ApathyDrive.Exit.move(spirit_or_monster_struct, "southwest")
  end
end
