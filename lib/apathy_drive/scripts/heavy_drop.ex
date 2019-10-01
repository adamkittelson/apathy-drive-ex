defmodule ApathyDrive.Scripts.HeavyDrop do
  alias ApathyDrive.{Character, Room}

  def execute(%Room{} = room, _mobile_ref, target_ref) do
    case room.mobiles[target_ref] do
      %Character{} = character ->
        items = character.inventory ++ character.equipment

        room =
          Enum.reduce(items, room, fn item, room ->
            if item.weight >= 150 do
              character = room.mobiles[target_ref]
              ApathyDrive.Commands.Drop.drop_item(room, character, item, false)
            else
              room
            end
          end)
          |> Room.load_items()

        character = Character.load_items(room.mobiles[target_ref])
        put_in(room.mobiles[character.ref], character)

      _ ->
        room
    end
  end
end
