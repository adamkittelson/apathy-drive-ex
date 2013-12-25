defmodule Systems.Command do

  def execute(player, text) do
    current_room    = Systems.Room.get_current_room(player)
    exit_directions = Systems.Room.exit_directions(current_room)

    if Enum.member? exit_directions, text do
      Systems.Room.move(player, text)
    else
      Players.send_message(player, ["scroll", "What?"])
    end

  end

end