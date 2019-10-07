defmodule ApathyDrive.Scripts.HeavyDrop do
  alias ApathyDrive.{Character, Room}

  def execute(%Room{} = room, _mobile_ref, target_ref) do
    case room.mobiles[target_ref] do
      %Character{} = character ->
        items = character.inventory ++ character.equipment

        Enum.reduce(items, room, fn item, room ->
          if item.weight >= 150 do
            character = room.mobiles[target_ref]
            ApathyDrive.Commands.Drop.drop_item(room, character, item)
          else
            room
          end
        end)

      _ ->
        room
    end
  end
end
