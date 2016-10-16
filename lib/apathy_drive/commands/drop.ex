defmodule ApathyDrive.Commands.Drop do
  use ApathyDrive.Command
  alias ApathyDrive.{Match, Repo, RoomUnity}

  def keywords, do: ["drop"]

  def execute(%Room{} = room, %Monster{} = monster, []) do
    Monster.send_scroll(monster, "<p>Drop what?</p>")
    room
  end

  def execute(%Room{room_unity: %RoomUnity{items: items}} = room, %Monster{spirit: %Spirit{inventory: inventory}} = monster, arguments) do
    item_name = Enum.join(arguments, " ")

    item = inventory
           |> Enum.map(&(%{name: &1["name"], keywords: String.split(&1["name"]), item: &1}))
           |> Match.one(:name_contains, item_name)

    case item do
      nil ->
        Monster.send_scroll(monster, "<p>You don't have \"#{item_name}\" to drop!</p>")

        room
      %{item: item} ->
        monster =
          put_in(monster.spirit.inventory, List.delete(inventory, item))

        room = put_in(room.monsters[monster.ref], monster)

        Monster.send_scroll(monster, "<p>You drop #{item["name"]}.</p>")

        put_in(room.room_unity.items, [item | items])
        |> Repo.save
    end
  end
end
