defmodule ApathyDrive.Commands.Get do
  use ApathyDrive.Command
  alias ApathyDrive.{Monster, Match, Repo, RoomUnity}

  def keywords, do: ["get"]

  def execute(%Room{} = room, %Monster{} = monster, []) do
    Monster.send_scroll(monster, "<p>Get what?</p>")
    room
  end

  def execute(%Room{room_unity: %RoomUnity{items: items}} = room, %Monster{} = monster, ["all"]) do
    items
    |> Enum.map(&(&1["name"]))
    |> get_all(room, monster)
  end

  def execute(%Room{room_unity: %RoomUnity{items: items}, item_descriptions: item_descriptions} = room, %Monster{spirit: %Spirit{inventory: inventory}} = monster, args) do
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
        Monster.send_scroll(monster, "<p>#{item_name.name |> capitalize_first} cannot be picked up.</p>")
        room
      actual_item ->
        monster = put_in(monster.spirit.inventory, [actual_item.item | inventory])

        Monster.send_scroll(monster, "<p>You get #{actual_item.name}.</p>")

        Monster.save(monster)

        room = put_in(room.room_unity.items, List.delete(room.room_unity.items, actual_item.item))
        room =
          put_in(room.monsters[monster.ref], monster)
          |> Repo.save
      true ->
        Monster.send_scroll(monster, "<p>You don't see \"#{item}\" here.</p>")
        room
    end
  end

  defp get_all([], room, monster) do
    Monster.send_scroll(monster, "<p>There is nothing here to get.</p>")
    room
  end

  defp get_all(item_names, room, monster) do
    item_names
    |> Enum.reduce(room, fn(item_name, updated_room) ->
         monster = updated_room.monsters[monster.ref]
         execute(updated_room, monster, [item_name])
       end)
  end
end
