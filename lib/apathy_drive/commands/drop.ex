defmodule ApathyDrive.Commands.Drop do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, CharacterItem, Item, Match, Mobile, Repo, RoomItem}

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
         %Item{characters_items_id: characters_items_id} = item ->

           {:ok, _} =
            Ecto.Multi.new
            |> Ecto.Multi.insert(:rooms_items, %RoomItem{room_id: room.id, item_id: item.id, level: item.level})
            |> Ecto.Multi.delete(:characters_items, %CharacterItem{id: characters_items_id})
            |> Repo.transaction

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
