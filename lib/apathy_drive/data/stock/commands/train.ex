defmodule Commands.Train do
  use Systems.Command

  def keywords, do: ["train"]

  def execute(spirit, nil, arguments) do
    send_message(spirit, "scroll", "<p>You need a body to do that.</p>")
  end

  def execute(spirit, monster, arguments) do
    current_room = Parent.of(spirit)

    cond do
      !Entity.has_component?(current_room, Components.Trainer) ->
        send_message(spirit, "scroll", "<p><span class='red'>You cannot TRAIN if you are not at a trainer!</span></p>")
      true ->
        Systems.Trainer.train(monster, current_room, arguments)
    end
  end
end
