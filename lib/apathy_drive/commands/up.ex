defmodule Commands.Up do
  use ApathyDrive.Command

  def keywords, do: ["u", "up"]

  def execute(spirit_or_monster_struct, _arguments) do
    ApathyDrive.Exit.move(spirit_or_monster_struct, "up")
  end
end
