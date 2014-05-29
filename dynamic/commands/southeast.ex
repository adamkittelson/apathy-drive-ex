defmodule Commands.SouthEast do
  use Systems.Command

  def keywords, do: ["se", "southeast"]

  def execute(entity, _arguments) do
    Systems.Room.move(entity, "southeast")
  end
end
