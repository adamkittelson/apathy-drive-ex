defmodule ApathyDrive.Commands.Reply do
  use ApathyDrive.Command
  alias ApathyDrive.{Directory, Mobile}

  def keywords, do: ["reply"]

  def execute(%Room{} = room, %Character{reply_to: nil} = character, _) do
    Mobile.send_scroll(character, "<p>Reply to who?</p>")
    room
  end

  def execute(%Room{} = room, %Character{} = character, message) do
    message =
      message
      |> Enum.join(" ")

    case Directory.find(character.reply_to) do
      {:local, name, room, ref} ->
        room
        |> RoomServer.find()
        |> RoomServer.tell(character.name, ref, message)

        Mobile.send_scroll(
          character,
          "<p><span class='red'>You tell #{name}:</span> #{Character.sanitize(message)}"
        )

      _ ->
        Mobile.send_scroll(character, "<p>#{character.reply_to} is no longer online.</p>")
    end

    room
  end
end
