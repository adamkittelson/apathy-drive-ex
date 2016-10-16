defmodule ApathyDrive.Commands.List do
  use ApathyDrive.Command

  def keywords, do: ["list"]

  def execute(%Room{} = room, %Monster{} = monster, _arguments) do
    Monster.send_scroll(monster, "<p><span class='dark-yellow'>You care not for mortal trinkets.</span></p>")
    room
  end
end
