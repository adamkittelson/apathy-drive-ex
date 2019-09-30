defmodule ApathyDrive.Scripts.Silvermere do
  alias ApathyDrive.{Mobile, Room}

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    messages = [
      "A guardsman shouts out the time of day.",
      "A voice shouts aloud \"Read the bulletin in the Adventurer's Guild!\"",
      "Children rush past you hopping around in youthful glee.",
      "A cheer of many voices can be heard in the distance.",
      "The awful sound of a drunken chorus echoes through the streets.",
      "A dog barks off in the distance."
    ]

    if :rand.uniform(100) > 77 do
      message = Enum.random(messages)
      Mobile.send_scroll(room.mobiles[mobile_ref], "<p>#{message}</p>")
    end

    room
  end
end
