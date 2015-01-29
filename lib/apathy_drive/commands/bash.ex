defmodule Commands.Bash do
  use ApathyDrive.Command

  def keywords, do: ["bash"]

  def execute(%Spirit{} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p>You need a body to do that.</p>")
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
          ApathyDrive.Exits.Door.bash(monster, current_room, room_exit)
        %{"kind" => "Gate"} ->
          ApathyDrive.Exits.Gate.bash(monster, current_room, room_exit)
        %{"kind" => "Key"} ->
          ApathyDrive.Exits.Key.bash(monster, current_room, room_exit)
        _ ->
          send_message(monster, "scroll", "<p>That exit has no door.</p>")
      end
    else
      send_message(monster, "scroll", "<p>Bash what?</p>")
    end
  end

end
