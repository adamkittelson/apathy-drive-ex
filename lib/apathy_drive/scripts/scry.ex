defmodule ApathyDrive.Scripts.Scry do
  alias ApathyDrive.{Ability, Room}

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    Room.update_mobile(room, mobile_ref, fn room, character ->
      Mobile.send_scroll(character, "<p>You scry!</p>")
    end)
  end
end
