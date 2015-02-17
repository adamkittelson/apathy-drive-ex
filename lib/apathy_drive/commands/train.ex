defmodule Commands.Train do
  use ApathyDrive.Command

  def keywords, do: ["train"]

  def execute(%Spirit{} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p>You need a body to do that.</p>")
  end

  def execute(%Monster{} = monster, arguments) do
    current_room = Monster.find_room(monster)

    if Room.trainer?(current_room) do
      Systems.Trainer.train(monster, current_room, arguments)
    else
      Monster.send_scroll(monster, "<p><span class='red'>You cannot TRAIN if you are not at a trainer!</span></p>")
    end
  end
end
