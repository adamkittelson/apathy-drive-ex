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

  def execute(player, [command | arguments]) do
    case Components.Login.get_step(player) do
      "intro" ->
        case command do
          "new" ->
            Components.Login.create_account_request_email(player)
          other ->
            Components.Login.sign_in_get_account(player, command)
        end
      "create_account_request_email" ->
        Components.Login.create_account_set_email(player, command)
      "create_account_request_password" ->
        Components.Login.create_account_set_password(player, command)
      "create_account_confirm_password" ->
        Components.Login.create_account_finish(player, command)
      "sign_in_check_password" ->
        Components.Login.sign_in_check_password(player, command)
      other ->
        execute_command(player, command, arguments)
    end
  end

  def execute_command(player, command, arguments) do
    command = @aliases[:"#{command}"] || command

    current_room = Systems.Room.get_current_room(player)
    if current_room do
      exit_directions = Systems.Room.exit_directions(current_room)
    end

    if Enum.member? @directions, command do
      if exit_directions && (Enum.member? exit_directions, command) do
        Systems.Room.move(player, command)
      else
        Players.send_message(player, ["scroll", "There is no exit in that direction."])
      end
    else
      Players.send_message(player, ["scroll", "What?"])
    end
  end

end
