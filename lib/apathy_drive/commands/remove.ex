defmodule ApathyDrive.Commands.Remove do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, EntityItem, Item, Match, Mobile, Repo}

  def keywords, do: ["remove", "unequip", "unwield"]

  def execute(%Room{} = room, %Character{} = character, []) do
    Mobile.send_scroll(character, "<p>Remove what?</p>")
    room
  end
  def execute(%Room{} = room, %Character{} = character, arguments) do
    item_name = Enum.join(arguments, " ")

    character.equipment
    |> Match.one(:name_contains, item_name)
    |> case do
         nil ->
           Mobile.send_scroll(character, "<p>You don't have \"#{item_name}\" equipped.</p>")
           room
         %Item{} = item_to_remove ->

           %EntityItem{id: item_to_remove.entities_items_id}
           |> Ecto.Changeset.change(%{equipped: false})
           |> Repo.update!

           Room.update_mobile(room, character.ref, fn(char) ->
             character =
               char
               |> Character.load_items
               |> Mobile.send_scroll("<p>You remove #{Item.colored_name(item_to_remove)}.</p>")

             send(character.socket, {:update_character, %{room_id: room.id, power: Mobile.power_at_level(character, character.level), level: character.level}})

             character
           end)
       end
  end

end
