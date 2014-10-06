defmodule Commands.Southeast do
  use Systems.Command

  def keywords, do: ["se", "southeast"]

  def execute(spirit, monster, _arguments) do
    Systems.Room.move(spirit, monster, "southeast")
  end
end
