defmodule ApathyDrive.Scripts do
  alias ApathyDrive.{Character, Item, ItemInstance, Repo, Room}

  def give_coins_up_to(%Room{} = room, mobile_ref, %{} = coins) do
    Room.update_mobile(room, mobile_ref, fn mobile ->
      coins =
        [:runic, :platinum, :gold, :silver, :copper]
        |> Enum.reduce(%{}, fn denomination, total ->
          current = Map.get(mobile, denomination)

          if additional = Map.get(coins, denomination) do
            Map.put(total, denomination, current + Enum.random(0..additional))
          else
            Map.put(total, denomination, current)
          end
        end)

      mobile
      |> Ecto.Changeset.change(coins)
      |> Repo.update!()
    end)
  end

  def give_item(%Room{} = room, mobile_ref, item_id) do
    item = Repo.get!(Item, item_id)

    Room.update_mobile(room, mobile_ref, fn mobile ->
      level = if item.type in ["Armour", "Weapon"], do: mobile.level, else: nil

      if item.weight <= Character.max_encumbrance(mobile) - Character.encumbrance(mobile) do
        %ItemInstance{
          item_id: item_id,
          room_id: nil,
          character_id: mobile.id,
          dropped_for_character_id: mobile.id,
          equipped: false,
          hidden: false,
          level: level
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
          hidden: false,
          level: level
        }
        |> Repo.insert!()

        Room.load_items(room)
      end
    end)
  end
end
