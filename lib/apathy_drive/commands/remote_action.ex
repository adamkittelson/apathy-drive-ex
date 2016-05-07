defmodule ApathyDrive.Commands.RemoteAction do
  alias ApathyDrive.{Doors, Room}

  def execute(%Room{} = room, %{"direction" => direction} = room_exit, from) do
    exit_to_trigger = exit_to_trigger(room, room_exit, from)

    room =
      room
      |> clear_triggers?(exit_to_trigger, room_exit)
      |> trigger(%{direction: exit_to_trigger["direction"], remote_exit: %{"direction" => direction, "room" => from}})

    if Doors.open?(room, exit_to_trigger) do
      if exit_to_trigger["message_when_revealed"] do
        Room.send_scroll(room, "<p><span class='white'>#{exit_to_trigger["message_when_revealed"]}</span></p>")
      end
    end
    room
  end

  defp trigger(room, exit_to_trigger) do
    room
    |> Systems.Effect.add(%{:triggered => exit_to_trigger, "stack_key" => exit_to_trigger, "stack_count" => 1}, 300)
  end

  defp exit_to_trigger(%Room{exits: exits}, %{"direction" => direction}, from) do
    exits
    |> Enum.find(fn(exit_to_trigger) ->
         exit_to_trigger
         |> Map.get("remote_action_exits", [])
         |> Enum.find(fn
              %{"room" => ^from, "direction" => ^direction} ->
                true
              _ ->
                false
            end)
       end)
  end

  def clear_triggers?(room, %{"remote_action_order_matters" => true, "direction" => direction}, %{"remote_action_order" => order}) do
    triggered_count =
      Doors.triggered?(room, direction)
      |> Enum.count

    if order != triggered_count + 1 do
      clear_triggers(room, direction)
    else
      room
    end
  end
  def clear_triggers?(room, _exit_to_trigger, _room_exit), do: room

  def clear_triggers(room, direction) do
    room.effects
    |> Map.keys
    |> Enum.filter(fn(key) ->
         room.effects[key][:triggered][:direction] == direction
       end)
    |> Enum.reduce(room, &(Systems.Effect.remove(&2, &1, show_expiration_message: true)))
  end
end
