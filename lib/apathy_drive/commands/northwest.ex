defmodule Commands.Northwest do
  use ApathyDrive.Command

  def keywords, do: ["nw", "northwest"]

  def execute(spirit_or_monster_struct, _arguments) do
    ApathyDrive.Exit.move(spirit_or_monster_struct, "northwest")
  end
end
