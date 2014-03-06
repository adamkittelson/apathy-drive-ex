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
    character = Components.Login.get_character(player)
    command = @aliases[:"#{command}"] || command

    current_room = Systems.Room.get_current_room(character)
    if current_room do
      exit_directions = Systems.Room.exit_directions(current_room)
    end

    if Enum.member? @directions, command do
      if exit_directions && (Enum.member? exit_directions, command) do
        Systems.Room.move(player, character, command)
      else
        Players.send_message(player, ["scroll", "<p>There is no exit in that direction.</p>"])
      end
    else
      Players.send_message(player, ["scroll", "<p>What?</p>"])
    end
    display_prompt(player)
  end

  def display_prompt(player) do
    Players.send_message(player, ["disable", "#prompt"])
    Players.send_message(player, ["disable", "#command"])
    Players.send_message(player, ["scroll", "<p><span id='prompt'>[HP=326/MA=76]:</span><input id='command' class='prompt'></input></p>"])
    Players.send_message(player, ["focus", "#command"])
  end

end
