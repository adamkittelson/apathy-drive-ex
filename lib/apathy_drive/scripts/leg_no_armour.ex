defmodule ApathyDrive.Scripts.LegNoArmour do
  alias ApathyDrive.{Character, Item, ItemInstance, Mobile, Repo, Room}

  def execute(%Room{} = room, _mobile_ref, target_ref) do
    Room.update_mobile(room, target_ref, fn
      room, %Character{} = character ->
        item = Enum.find(character.equipment, &(&1.worn_on == "Legs"))

        if item do
          ItemInstance
          |> Repo.get(item.instance_id)
          |> Repo.delete!()

          Mobile.send_scroll(
            character,
            "<p>Your #{Item.colored_name(item, character: character)} are vaporized!</p>"
          )

          Room.send_scroll(
            room,
            "<p>#{Mobile.colored_name(character)}'s #{
              Item.colored_name(item, character: character)
            } are vaporized!</p>",
            [character]
          )

          update_in(character.equipment, &List.delete(&1, item))
        else
          room
        end

      room, _ ->
        room
    end)
  end
end
