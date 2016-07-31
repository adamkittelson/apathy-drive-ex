defmodule ApathyDrive.Commands.List do
  use ApathyDrive.Command

  def keywords, do: ["list"]

  def execute(%Room{} = room, %Mobile{} = mobile, _arguments) do
    Mobile.send_scroll(mobile, "<p><span class='dark-yellow'>You care not for mortal trinkets.</span></p>")
    room
  end
end
