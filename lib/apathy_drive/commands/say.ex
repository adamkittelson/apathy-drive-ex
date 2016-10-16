defmodule ApathyDrive.Commands.Say do
  use ApathyDrive.Command

  def keywords, do: ["say"]

  def execute(%Room{} = room, %Monster{monster_template_id: nil} = monster, _message) do
    Monster.body_required(monster)

    room
  end

  def execute(%Room{} = room, %Monster{} = monster, args) do
    message =
      args
      |> Enum.join(" ")
      |> Monster.sanitize()

    Room.send_scroll(room, "<p>#{Monster.look_name(monster)} says: <span class='dark-green'>\"#{message}\"</span></p>", monster)
    Monster.send_scroll(monster, "<p>You say: <span class='dark-green'>\"#{message}\"</span></p>")
    room
  end

end
