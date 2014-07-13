defmodule Commands.List do
  use Systems.Command

  def keywords, do: ["list"]

  def execute(entity, _arguments) do
    room = Parent.of(entity)
    cond do
      Entity.has_component?(room, Components.Shop) ->
        Systems.Shop.list(entity, room)
      Entity.has_component?(room, Components.Trainer) ->
        Systems.Trainer.list(entity, room)
      true ->
        send_message(entity, "scroll", "<p><span class='red'>You cannot LIST if you are not in a shop!</span></p>")
    end
  end
end
