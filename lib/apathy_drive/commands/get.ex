defmodule ApathyDrive.Commands.Get do
  use ApathyDrive.Command
  alias ApathyDrive.{Mobile, Match, Repo, RoomUnity}

  def keywords, do: ["get"]

  def execute(%Room{} = room, %Mobile{} = mobile, []) do
    Mobile.send_scroll(mobile, "<p>Get what?</p>")
    room
  end

  def execute(%Room{room_unity: %RoomUnity{items: items}} = room, %Mobile{} = mobile, "all") do
    items
    |> Enum.map(&(&1["name"]))
    |> get_all(room, mobile)
  end

  def execute(%Room{room_unity: %RoomUnity{items: items}, item_descriptions: item_descriptions} = room, %Mobile{spirit: %Spirit{inventory: inventory}} = mobile, args) do
    item = Enum.join(args, " ")

    actual_item = items
                  |> Enum.map(&(%{name: &1["name"], keywords: String.split(&1["name"]), item: &1}))
                  |> Match.one(:name_contains, item)

    visible_item = item_descriptions["visible"]
                   |> Map.keys
                   |> Enum.map(&(%{name: &1, keywords: String.split(&1)}))
                   |> Match.one(:keyword_starts_with, item)

    hidden_item = item_descriptions["hidden"]
                  |> Map.keys
                  |> Enum.map(&(%{name: &1, keywords: String.split(&1)}))
                  |> Match.one(:keyword_starts_with, item)

    cond do
      item_name = visible_item || hidden_item ->
        Mobile.send_scroll(mobile, "<p>#{item_name.name |> capitalize_first} cannot be picked up.</p>")
        room
      actual_item ->
        mobile = put_in(mobile.spirit.inventory, [actual_item.item | inventory])

        Mobile.send_scroll(mobile, "<p>You get #{actual_item.name}.</p>")

        Mobile.save(mobile)

        room = put_in(room.room_unity.items, List.delete(room.room_unity.items, actual_item.item))
        room =
          put_in(room.mobiles[mobile.ref], mobile)
          |> Repo.save!
      true ->
        Mobile.send_scroll(mobile, "<p>You don't see \"#{item}\" here.</p>")
        room
    end
  end

  defp get_all([], room, mobile) do
    Mobile.send_scroll(mobile, "<p>There is nothing here to get.</p>")
    room
  end

  defp get_all(item_names, room, mobile) do
    item_names
    |> Enum.reduce(room, fn(item_name, updated_room) ->
         mobile = updated_room.mobiles[mobile.ref]
         execute(updated_room, mobile, [item_name])
       end)
  end
end
