defmodule Commands.Train do
  use ApathyDrive.Command

  def keywords, do: ["train"]

  def execute(%Spirit{} = spirit, arguments) do
    current_room = Spirit.find_room(spirit)

    if Room.trainer?(current_room) do
      Systems.Trainer.train(spirit, current_room, arguments)
    else
      Spirit.send_scroll(spirit, "<p><span class='red'>You cannot TRAIN if you are not at a trainer!</span></p>")
    end
  end

  def execute(%Monster{} = monster, _arguments) do
    monster
    |> Monster.send_scroll("<p>Only spirits may train.</p>")
  end
end
