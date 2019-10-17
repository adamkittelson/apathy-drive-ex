defmodule ApathyDrive.Scripts.GooJanitor do
  alias ApathyDrive.{Mobile, Room}

  def execute(%Room{} = room, _mobile_ref, target_ref) do
    send(self(), {:goo_janitor, Mobile.colored_name(room.mobiles[target_ref])})
    room
  end
end
