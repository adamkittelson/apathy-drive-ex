defmodule Commands.Southwest do
  use Systems.Command

  def keywords, do: ["sw", "southwest"]

  def execute(entity, _arguments) do
    Systems.Room.move(entity, "southwest")
  end
end
