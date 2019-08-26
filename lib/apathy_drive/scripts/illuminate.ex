defmodule ApathyDrive.Scripts.Illuminate do
  use ApathyDrive.Scripts

  def execute(%Room{} = room, mobile_ref) do
    # light ball
    item_id = 1085

    give_item(room, mobile_ref, item_id)
  end
end
