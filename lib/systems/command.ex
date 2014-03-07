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

  def execute(player, command, arguments) do
    command_found = false
    character = Components.Login.get_character(player)
    command = @aliases[:"#{command}"] || command

    current_room = Systems.Room.get_current_room(character)
    if current_room do
      exit_directions = Systems.Room.exit_directions(current_room)
    end

    if Enum.member? @directions, command do
      command_found = true
      if exit_directions && (Enum.member? exit_directions, command) do
        Systems.Room.move(player, character, command)
      else
        Players.send_message(player, ["scroll", "<p>There is no exit in that direction.</p>"])
      end
    end

    if Enum.member?(["l", "look"], command) do
      command_found = true
      if Enum.any? arguments do
        if target_character = Systems.Room.find_character_by_name(current_room, Enum.join(arguments, " ")) do
          Systems.CharacterDescription.add_character_description_to_scroll(player, target_character)
        else
          Players.send_message(player, ["scroll", "<p>You do not notice that here.</p>"])
        end
      else
        Systems.Room.display_room_in_scroll(player, current_room)
      end
    end

    if command == "help" do
      command_found = true
      Systems.Command.help(player, arguments)
    end

    if !command_found do
      Players.send_message(player, ["scroll", "<p>What?</p>"])
    end
    display_prompt(player, character)
  end

  def display_prompt(player, character) do
    Players.send_message(player, ["disable", "#prompt"])
    Players.send_message(player, ["disable", "#command"])
    Players.send_message(player, ["scroll", "<p><span id='prompt'>[HP=#{Components.HP.value(character)}]:</span><input id='command' class='prompt'></input></p>"])
    Players.send_message(player, ["focus", "#command"])
  end

end
