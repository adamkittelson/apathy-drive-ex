defmodule ApathyDrive.Commands.Tell do
  use ApathyDrive.Command
  alias ApathyDrive.{Directory, Mobile}

  def keywords, do: ["tell", "whisper"]

  def execute(%Room{} = room, %Character{} = character, [player | message]) do
    message =
      message
      |> Enum.join(" ")

    case Directory.find(player) do
      {:local, name, room, ref} ->
        room
        |> RoomServer.find()
        |> RoomServer.tell(character.name, ref, message)

        Mobile.send_scroll(
          character,
          "<p><span class='red'>You tell #{name}:</span> #{Character.sanitize(message)}"
        )

      _ ->
        Mobile.send_scroll(character, "<p>Who?</p>")
    end

    room
  end

  def execute(%Room{} = room, %Character{} = character, _) do
    Mobile.send_scroll(character, "<p>Who?</p>")
    room
  end
end
