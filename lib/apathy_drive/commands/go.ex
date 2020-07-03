defmodule ApathyDrive.Commands.Go do
  use ApathyDrive.Command
  alias ApathyDrive.Character

  def keywords, do: ["go"]

  # this is a dummy command so things like "go path" when in a room that doesn't have an action exit
  # don't result in erroneous gossips
  def execute(%Room{} = room, %Character{} = character, _) do
    Mobile.send_scroll(character, "<p>What?</p>")
    room
  end
end
