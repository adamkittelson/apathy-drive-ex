defmodule Commands.List do
  use Systems.Command

  def keywords, do: ["list"]

  def execute(spirit, monster, _arguments) do
    room = Parent.of(spirit)
    cond do
      Entity.has_component?(room, Components.Shop) ->
        Systems.Shop.list(spirit, monster, room)
      Entity.has_component?(room, Components.Trainer) ->
        Systems.Trainer.list(spirit, monster, room)
      true ->
        send_message(spirit, "scroll", "<p><span class='red'>You cannot LIST if you are not in a shop!</span></p>")
    end
  end
end
