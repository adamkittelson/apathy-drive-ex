defmodule Commands.Train do
  use Systems.Command

  def keywords, do: ["train"]

  def execute(entity, arguments) do
    current_room = Components.CurrentRoom.get_current_room(entity)

    cond do
      !Entity.has_component?(current_room, Components.Trainer) ->
        Components.Player.send_message(entity, ["scroll", "<p><span class='red'>You cannot TRAIN if you are not at a trainer!</span></p>"])
      Components.Spirit.value(entity) ->
        Components.Player.send_message(entity, ["scroll", "<p>You need a body to do that.</p>"])
      true ->
        Systems.Trainer.train(entity, current_room, arguments)
    end
  end
end
