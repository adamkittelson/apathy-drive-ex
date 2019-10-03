defmodule ApathyDrive.Scripts.MeltArmour do
  alias ApathyDrive.{Character, Item, ItemInstance, Mobile, Repo, Room}

  def execute(%Room{} = room, _mobile_ref, target_ref) do
    Room.update_mobile(room, target_ref, fn
      _room, %Character{} = character ->
        Enum.each(character.equipment, fn item ->
          if item.type == "Armour" do
            Mobile.send_scroll(
              character,
              "<p>#{Item.colored_name(item, character: character)} <span class='red'>melts away!</span></p>"
            )

            ItemInstance
            |> Repo.get(item.instance_id)
            |> Repo.delete!()
          end
        end)

        Character.load_items(character)

      room, _ ->
        room
    end)
  end
end
