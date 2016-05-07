defmodule ApathyDrive.Commands.Get do
  use ApathyDrive.Command
  alias ApathyDrive.{Mobile, Match, Repo}

  def keywords, do: ["get"]

  def execute(mobile, []) do
    Mobile.send_scroll(mobile, "<p>Get what?</p>")
  end

  def execute(mobile, args) when is_pid(mobile) do
    item = Enum.join(args, " ")
    Mobile.get_item(mobile, item)
  end

  def execute(%Mobile{room_id: room_id, spirit: %Spirit{inventory: inventory}} = mobile, %{"weight" => weight} = item) do
    if Mobile.remaining_encumbrance(mobile) >= weight do
      mobile =
        put_in(mobile.spirit.inventory, [item | inventory])

      Mobile.send_scroll(mobile, "<p>You get #{item["name"]}.</p>")

      Mobile.save(mobile)
    else
      room_id
      |> Room.find
      |> Room.add_item(item)

      Mobile.send_scroll(mobile, "<p>#{capitalize_first(item["name"])} is too heavy.</p>")
    end
  end

  def execute(%Mobile{room_id: room_id}, item) do
    room_id
    |> RoomServer.find
    |> RoomServer.get_item(self, item)
  end

  def execute(%Room{items: items} = room, mobile, "all") do
    items
    |> Enum.map(&(&1["name"]))
    |> get_all(mobile)

    room
  end

  def execute(%Room{items: items, item_descriptions: item_descriptions} = room, mobile, item) do
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
        Mobile.send_scroll(mobile, "<p>#{item_name |> capitalize_first} cannot be picked up.</p>")
        room
      actual_item ->
        Mobile.get_item(mobile, actual_item.item)

        room
        |> Map.put(:items, List.delete(room.items, actual_item.item))
        |> Repo.save!
      true ->
        Mobile.send_scroll(mobile, "<p>You don't see \"#{item}\" here.</p>")
        room
    end
  end

  defp get_all([], mobile) do
    Mobile.send_scroll(mobile, "<p>There is nothing here to get.</p>")
  end

  defp get_all(item_names, mobile) do
    Enum.each(item_names, &Room.get_item(self, mobile, &1))
  end
end
