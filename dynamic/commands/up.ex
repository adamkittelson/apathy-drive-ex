defmodule Commands.Up do
  use Systems.Command

  def keywords, do: ["u", "up"]

  def execute(entity, _arguments) do
    Systems.Room.move(entity, "up")
  end
end
