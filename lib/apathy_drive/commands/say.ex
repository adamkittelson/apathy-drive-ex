defmodule ApathyDrive.Commands.Say do
  use ApathyDrive.Command

  def keywords, do: ["say"]

  def execute(%Room{} = room, %Character{} = character, args) do
    message =
      args
      |> Enum.join(" ")
      |> Monster.sanitize()

    Room.send_scroll(room, "<p>#{Mobile.look_name(character)} says: <span class='dark-green'>\"#{message}\"</span></p>", [character])
    Mobile.send_scroll(character, "<p>You say: <span class='dark-green'>\"#{message}\"</span></p>")
    room
  end

end
