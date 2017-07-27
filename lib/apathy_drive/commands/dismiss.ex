defmodule ApathyDrive.Commands.Dismiss do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Companion}

  def keywords, do: ["dimiss", "fire"]

  def execute(%Room{} = room, %Character{} = character, arguments) do
    query = Enum.join(arguments)

    case Room.find_mobile_in_room(room, character, query) do
      %Companion{} = companion ->
        Companion.dismiss(companion, room)
      _ ->
        Mobile.send_scroll(character, "<p>You don't see #{query} here!</p>")
        room
    end
  end

end
