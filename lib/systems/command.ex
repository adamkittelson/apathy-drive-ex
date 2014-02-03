defmodule Systems.Command do

  @aliases [ u: "up",
             d: "down",
             n: "north",
             ne: "northeast",
             e:  "east",
             se: "southeast",
             s:  "south",
             sw: "southwest",
             w:  "west",
             nw: "northwest" ]

  @directions [ "up",
                "down",
                "north",
                "northeast",
                "east",
                "southeast",
                "south",
                "southwest",
                "west",
                "northwest" ]

  def help(player, arguments) do
    keyword = Enum.join(arguments, " ")
    help_info = Systems.Help.find(keyword)
    if help_info do
      Players.send_message(player, ["scroll", help_info])
    else
      Players.send_message(player, ["scroll", "<p>Sorry, no help is available for \"#{Enum.join(arguments, " ")}\".</p>"])
    end
  end

  def execute(player, command, _arguments) do
    command = @aliases[:"#{command}"] || command

    current_room = Systems.Room.get_current_room(player)
    if current_room do
      exit_directions = Systems.Room.exit_directions(current_room)
    end

    if Enum.member? @directions, command do
      if exit_directions && (Enum.member? exit_directions, command) do
        Systems.Room.move(player, command)
      else
        Players.send_message(player, ["scroll", "<p>There is no exit in that direction.</p>"])
      end
    else
      Players.send_message(player, ["scroll", "<p>What?</p>"])
    end
  end

end
