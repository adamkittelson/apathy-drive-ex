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

    display_prompt(character)

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
        if target = current_room |> find_entity_in_room(Enum.join(arguments, " ")) do
          Systems.Description.add_description_to_scroll(character, target)
        else
          Players.send_message(player, ["scroll", "<p>You do not notice that here.</p>"])
        end
      else
        Systems.Room.display_room_in_scroll(character, current_room)
      end
    end

    if command == "help" do
      command_found = true
      Systems.Command.help(player, arguments)
    end

    if !command_found do
      Players.send_message(player, ["scroll", "<p>What?</p>"])
    end
  end

  def find_entity_in_room(room, string) do
    room |> Systems.Room.entities_in_room |> Systems.Match.first(:name_contains, string)
  end

  def display_prompt(character) do
    Components.Player.send_message(character, ["disable", "#prompt"])
    Components.Player.send_message(character, ["disable", "#command"])
    Components.Player.send_message(character, ["scroll", "<p><span id='prompt'>[HP=#{Components.HP.value(character)}]:</span><input id='command' class='prompt'></input></p>"])
    Components.Player.send_message(character, ["focus", "#command"])
  end

end
