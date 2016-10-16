defmodule ApathyDrive.Commands.Search do
  use ApathyDrive.Command

  def keywords, do: ["sea", "search"]

  def execute(%Room{} = room, %Monster{} = monster, []) do
    Monster.send_scroll(monster, "<p>Your search revealed nothing.</p>")
    room
  end

  def execute(%Room{} = room, %Monster{} = monster, arguments) do
    direction = arguments
                |> Enum.join(" ")
                |> Room.direction

    room_exit = Room.get_exit(room, direction)

    case room_exit do
      %{"kind" => "Hidden"} ->
        search(room, monster, room_exit)
      _ ->
        Monster.send_scroll(monster, "<p>You notice nothing different #{ApathyDrive.Exit.direction_description(direction)}.</p>")
        room
    end
  end

  defp search(%Room{} = room, %Monster{} = monster, %{"direction" => direction, "searchable" => searchable?} = room_exit) do
    if Room.searched?(room, room_exit) || !searchable? || :rand.uniform(100) < 75 do
      Monster.send_scroll(monster, "<p>You notice nothing different #{ApathyDrive.Exit.direction_description(direction)}.</p>")
      room
    else
      if message = room_exit["message_when_revealed"] do
        Monster.send_scroll(monster, "<p>#{message}</p>")
      else
        Monster.send_scroll(monster, "<p>You found an exit #{ApathyDrive.Exit.direction_description(direction)}!</p>")
      end

      Systems.Effect.add(room, %{searched: direction}, 300)
    end
  end

end
