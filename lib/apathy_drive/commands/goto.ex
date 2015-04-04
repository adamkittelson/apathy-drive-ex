defmodule Commands.Goto do
  use ApathyDrive.Command

  def keywords, do: ["goto"]

  def execute(%Spirit{} = spirit, arguments) do
    case find_room(arguments) do
      nil ->
        Spirit.send_scroll(spirit, "<p>There is no room with that id.</p>")
      room ->
        room = Room.value(room)

        Room.look(room, spirit)

        spirit
        |> Spirit.set_room_id(room.id)
        |> Spirit.deactivate_hint("movement")
        |> Spirit.save
    end
  end

  def execute(%Monster{} = monster, arguments) do
    case find_room(arguments) do
      nil ->
        Monster.send_scroll(monster, "<p>There is no room with that id.</p>")
      room ->
        room = Room.value(room)

        Room.look(room, monster)

        monster = monster
                  |> Monster.set_room_id(room.id)
                  |> Monster.save

        Room.look(room, monster)

        monster
    end
  end

  def find_room(arguments) do
    arguments
    |> Enum.join
    |> String.to_integer
    |> Room.find
  end

end
