defmodule ApathyDrive.Commands.Emote do
  use ApathyDrive.Command
  alias ApathyDrive.Mobile

  def keywords, do: ["emote"]

  def execute(%Room{} = room, %Character{} = character, []) do
    Mobile.send_scroll(character, "<p>Emote what?</p>")
    room
  end

  def execute(%Room{} = room, %Character{} = character, args) do
    raw_message =
      args
      |> Enum.join(" ")
      |> Character.sanitize()

    message =
      "<p><span style='color:lightskyblue'><i>#{character.name} #{raw_message}</i></span></p>"

    Room.send_scroll(room, message)

    room
  end
end
