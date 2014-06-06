defmodule Commands.Train do
  use Systems.Command

  def keywords, do: ["train"]

  def execute(entity, arguments) do
    current_room = Components.CurrentRoom.get_current_room(entity)
    if Entity.has_component?(current_room, Components.Trainer) do
      Systems.Trainer.train(entity, current_room, arguments)
    else
      Components.Player.send_message(entity, ["scroll", "<p><span class='red'>You cannot TRAIN if you are not at a trainer!</span></p>"])
    end
  end
end
