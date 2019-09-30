defmodule ApathyDrive.Scripts.Illuminate do
  import ApathyDrive.Scripts
  alias ApathyDrive.Room

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    # light ball
    item_id = 1085

    give_item(room, mobile_ref, item_id)
  end
end
