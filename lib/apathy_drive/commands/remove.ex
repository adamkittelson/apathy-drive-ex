defmodule ApathyDrive.Commands.Remove do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Item, ItemInstance, Match, Mobile, Repo}

  def keywords, do: ["remove", "unequip", "unwield", "rem"]

  def execute(%Room{} = room, %Character{} = character, []) do
    Mobile.send_scroll(character, "<p>Remove what?</p>")
    room
  end

  def execute(%Room{} = room, %Character{ref: ref} = character, ["all"]) do
    character.equipment
    |> Enum.map(& &1.name)
    |> Enum.reduce(room, fn item_name, updated_room ->
      character = updated_room.mobiles[ref]
      execute(updated_room, character, [item_name])
    end)
  end

  def execute(%Room{} = room, %Character{} = character, arguments) do
    item_name = Enum.join(arguments, " ")

    character.equipment
    |> Match.one(:name_contains, item_name)
    |> case do
      nil ->
        Mobile.send_scroll(character, "<p>You don't have \"#{item_name}\" equipped.</p>")
        room

      %{} = item_to_remove ->
        ItemInstance
        |> Repo.get(item_to_remove.instance_id)
        |> Ecto.Changeset.change(%{equipped: false, class_id: nil})
        |> Repo.update!()

        room =
          Room.update_mobile(room, character.ref, fn room, char ->
            char = update_in(char.equipment, &List.delete(&1, item_to_remove))

            item_to_remove =
              item_to_remove
              |> Map.put(:equipped, false)

            char =
              update_in(char.inventory, &[item_to_remove | &1])
              |> Character.add_equipped_items_effects()
              |> Character.load_abilities()

            if item_to_remove.type == "Light" do
              Mobile.send_scroll(
                char,
                "<p>You remove the #{Item.colored_name(item_to_remove)} and extinguish it.</p>"
              )

              char_ref = character.ref

              Enum.each(room.mobiles, fn
                {ref, %Character{} = mobile} when ref != char_ref ->
                  Mobile.send_scroll(
                    mobile,
                    "<p>#{Mobile.colored_name(character)} removes the #{
                      Item.colored_name(item_to_remove, character: mobile)
                    } and extinguishes it.</p>"
                  )

                _ ->
                  :noop
              end)
            else
              Mobile.send_scroll(
                char,
                "<p>You remove #{Item.colored_name(item_to_remove, character: char)}.</p>"
              )

              char_ref = character.ref

              Enum.each(room.mobiles, fn
                {ref, %Character{} = mobile} when ref != char_ref ->
                  Mobile.send_scroll(
                    mobile,
                    "<p>#{Mobile.colored_name(character)} removes the #{
                      Item.colored_name(item_to_remove, character: mobile)
                    }.</p>"
                  )

                _ ->
                  :noop
              end)
            end

            send(
              char.socket,
              {:update_character,
               %{
                 room_id: room.id,
                 power: Mobile.power_at_level(char, char.level),
                 level: char.level
               }}
            )

            char
          end)

        Room.update_hp_bar(room, character.ref)
        Room.update_mana_bar(room, character.ref)
        room
    end
  end
end
