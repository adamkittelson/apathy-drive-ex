defmodule ApathyDrive.Scripts.DropAll do
  alias ApathyDrive.{Character, Room}

  def execute(%Room{} = room, _mobile_ref, target_ref) do
    case room.mobiles[target_ref] do
      %Character{} = character ->
        Enum.reduce(character.inventory, room, fn item, room ->
          character = room.mobiles[target_ref]
          ApathyDrive.Commands.Drop.drop_item(room, character, item)
        end)

      _ ->
        room
    end
  end
end
