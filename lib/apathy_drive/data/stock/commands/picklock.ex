defmodule Commands.Picklock do
  use Systems.Command

  def keywords, do: ["picklock", "pick", "pi"]

  def execute(spirit, nil, _arguments) do
    send_message(spirit, "scroll", "<p>You need a body to do that.</p>")
  end

  def execute(_spirit, monster, arguments) do
    current_room = Parent.of(monster)

    if Enum.any? arguments do
      direction = arguments
                  |> Enum.join(" ")
                  |> ApathyDrive.Exit.direction

      room_exit = ApathyDrive.Exit.get_exit_by_direction(current_room, direction)

      case room_exit do
        nil ->
          send_message(monster, "scroll", "<p>There is no exit in that direction!</p>")
        %{"kind" => "Door"} ->
          ApathyDrive.Exits.Door.pick(monster, current_room, room_exit)
        %{"kind" => "Gate"} ->
          ApathyDrive.Exits.Gate.pick(monster, current_room, room_exit)
        %{"kind" => "Key"} ->
          ApathyDrive.Exits.Key.pick(monster, current_room, room_exit)
        _ ->
          send_message(monster, "scroll", "<p>That exit has no door.</p>")
      end
    else
      send_message(monster, "scroll", "<p>Pick what?</p>")
    end
  end

end
