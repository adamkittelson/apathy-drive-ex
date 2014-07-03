defmodule Commands.List do
  use Systems.Command

  def keywords, do: ["list"]

  def execute(entity, _arguments) do
    current_room = Components.CurrentRoom.get_current_room(entity)
    cond do
      Entity.has_component?(current_room, Components.Shop) ->
        Systems.Shop.list(entity, current_room)
      Entity.has_component?(current_room, Components.Trainer) ->
        Systems.Trainer.list(entity, current_room)
      true ->
        send_message(entity, "scroll", "<p><span class='red'>You cannot SELL if you are not in a shop!</span></p>")
    end
  end
end
