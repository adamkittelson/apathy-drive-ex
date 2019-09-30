defmodule ApathyDrive.Scripts.MeltInv do
  alias ApathyDrive.{Character, ItemInstance, Repo, Room}

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    Room.update_mobile(room, mobile_ref, fn
      %Character{} = character ->
        Enum.each(character.inventory, fn item ->
          ItemInstance
          |> Repo.get(item.instance_id)
          |> Repo.delete!()
        end)

        Character.load_items(character)

      _ ->
        room
    end)
  end
end
