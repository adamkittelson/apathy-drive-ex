defmodule ApathyDrive.Commands.Remove do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Item, Match, Mobile, Repo}

  def keywords, do: ["remove", "unequip", "unwield"]

  def execute(%Room{} = room, %Character{} = character, []) do
    Mobile.send_scroll(character, "<p>Remove what?</p>")
    room
  end
  def execute(%Room{} = room, %Character{} = character, arguments) do
    item_name = Enum.join(arguments, " ")

    character.equipment
    |> Enum.map(&(%{name: &1.item.name, item: &1}))
    |> Match.one(:name_contains, item_name)
    |> case do
         nil ->
           Mobile.send_scroll(character, "<p>You don't have \"#{item_name}\" equipped.</p>")
           room
         %{item: item_to_remove} ->

           item_to_remove
           |> Map.put(:equipped, false)
           |> Repo.save!

           Room.update_mobile(room, character.ref, fn(char) ->
             char
             |> Repo.preload([characters_items: :item], [force: true])
             |> Mobile.send_scroll("<p>You remove #{Item.colored_name(item_to_remove.item)}.</p>")
           end)
       end
  end

end
