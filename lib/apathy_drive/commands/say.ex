defmodule ApathyDrive.Commands.Say do
  use ApathyDrive.Command

  def keywords, do: ["say"]

  def execute(%Room{} = room, %Mobile{monster_template_id: nil} = mobile, _message) do
    Mobile.body_required(mobile)

    room
  end

  def execute(%Room{} = room, %Mobile{} = mobile, args) do
    message =
      args
      |> Enum.join(" ")
      |> Mobile.sanitize()

    Room.send_scroll(room, "<p>#{Mobile.look_name(mobile)} says: <span class='dark-green'>\"#{message}\"</span></p>", mobile)
    Mobile.send_scroll(mobile, "<p>You say: <span class='dark-green'>\"#{message}\"</span></p>")
    room
  end

end
