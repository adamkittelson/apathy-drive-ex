defmodule Commands.Northwest do
  use Systems.Command

  def keywords, do: ["nw", "northwest"]

  def execute(entity, _arguments) do
    Systems.Room.move(entity, "northwest")
  end
end
