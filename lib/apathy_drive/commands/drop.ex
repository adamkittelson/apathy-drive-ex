defmodule ApathyDrive.Commands.Drop do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, EntityItem, Item, Match, Mobile, Repo}

  def keywords, do: ["drop"]

  def execute(%Room{} = room, %Character{} = character, []) do
    Mobile.send_scroll(character, "<p>Drop what?</p>")
    room
  end

  def execute(%Room{} = room, %Character{inventory: inventory} = character, arguments) do
    item_name = Enum.join(arguments, " ")

    inventory
    |> Match.one(:name_contains, item_name)
    |> case do
         nil ->
           Mobile.send_scroll(character, "<p>You don't have \"#{item_name}\" to drop!</p>")
           room
         %Item{entities_items_id: entities_items_id} = item ->

           %EntityItem{id: entities_items_id}
           |> Ecto.Changeset.change(%{assoc_table: "rooms", assoc_id: room.id})
           |> Repo.update!

           room
           |> Room.load_items
           |> Room.update_mobile(character.ref, fn(char) ->
                char
                |> Character.load_items
                |> Mobile.send_scroll("<p>You drop #{Item.colored_name(item)}.</p>")
              end)
      end
  end
end
