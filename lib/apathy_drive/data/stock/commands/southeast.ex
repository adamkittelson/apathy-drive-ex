defmodule Commands.Southeast do
  use Systems.Command

  def keywords, do: ["se", "southeast"]

  def execute(spirit_or_monster_struct, _arguments) do
    ApathyDrive.Exit.move(spirit_or_monster_struct, "southeast")
  end
end
