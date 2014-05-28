defmodule Commands.List do
  use Systems.Command

  def keywords, do: ["list"]

  def execute(entity, _arguments) do
    current_room = Components.CurrentRoom.get_current_room(entity)
    Systems.Shop.list(entity, current_room)
  end
end
