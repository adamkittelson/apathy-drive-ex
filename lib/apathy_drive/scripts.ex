defmodule ApathyDrive.Scripts do
  defmacro __using__(_opts) do
    quote do
      alias ApathyDrive.{Character, Item, ItemInstance, Mobile, Repo, Room}

      def give_item(%Room{} = room, mobile_ref, item_id) do
        item = Repo.get!(Item, item_id)

        Room.update_mobile(room, mobile_ref, fn mobile ->
          if item.weight <= Character.max_encumbrance(mobile) - Character.encumbrance(mobile) do
            %ItemInstance{
              item_id: item_id,
              room_id: nil,
              character_id: mobile.id,
              dropped_for_character_id: mobile.id,
              equipped: false,
              hidden: false
            }
            |> Repo.insert!()

            Character.load_items(mobile)
          else
            %ItemInstance{
              item_id: item_id,
              room_id: room.id,
              character_id: nil,
              dropped_for_character_id: nil,
              equipped: false,
              hidden: false
            }
            |> Repo.insert!()

            Room.load_items(room)
          end
        end)
      end
    end
  end
end
