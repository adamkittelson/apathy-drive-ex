defmodule ApathyDrive.TimerManager do
  alias ApathyDrive.{Character, Room, RoomServer}
  require Logger

  def seconds(seconds), do: seconds |> :timer.seconds() |> trunc

  def send_after(%{timers: timers} = entity, {name, time, term}) do
    send_at = time + (DateTime.utc_now() |> DateTime.to_unix(:millisecond) |> trunc)

    timers = Map.put(timers, name, %{send_at: send_at, message: term})

    send(self(), :start_timer)

    Map.put(entity, :timers, timers)
  end

  def apply_timers(%Room{timers: timers} = room) do
    now = DateTime.utc_now() |> DateTime.to_unix(:millisecond)

    room =
      timers
      |> Enum.reduce(room, fn {name, %{send_at: send_at, message: message}}, updated_room ->
        if send_at < now do
          updated_room = update_in(updated_room.timers, &Map.delete(&1, name))

          {:noreply, updated_room} = RoomServer.handle_info(message, updated_room)
          updated_room
        else
          updated_room
        end
      end)

    Enum.reduce(room.items, room, fn item, room ->
      item.timers
      |> Enum.reduce(room, fn {name, %{send_at: send_at, message: message}}, room ->
        if send_at < now do
          item = update_in(item.timers, &Map.delete(&1, name))

          location =
            Enum.find_index(
              room.items,
              &(&1.instance_id == item.instance_id)
            )

          update_in(room.items, &List.replace_at(&1, location, item))

          {:noreply, room} = RoomServer.handle_info(message, room)
          room
        else
          room
        end
      end)
    end)
  end

  def apply_timers(%Room{} = room, mobile_ref) do
    now = DateTime.utc_now() |> DateTime.to_unix(:millisecond)

    room =
      Room.update_mobile(room, mobile_ref, fn room, %{timers: timers} ->
        timers
        |> Enum.reduce(room, fn {name, %{send_at: send_at, message: message}}, updated_room ->
          # handling the previous message may have removed the mobile from the room
          if updated_room.mobiles[mobile_ref] && send_at < now do
            updated_room =
              update_in(updated_room.mobiles[mobile_ref].timers, &Map.delete(&1, name))

            {:noreply, updated_room} = RoomServer.handle_info(message, updated_room)
            updated_room
          else
            updated_room
          end
        end)
      end)

    Room.update_mobile(room, mobile_ref, fn
      room, %Character{} = mobile ->
        room =
          Enum.reduce(mobile.inventory, room, fn item, room ->
            item.timers
            |> Enum.reduce(room, fn {name, %{send_at: send_at, message: message}}, room ->
              if send_at < now do
                item = update_in(item.timers, &Map.delete(&1, name))

                location =
                  Enum.find_index(
                    mobile.inventory,
                    &(&1.instance_id == item.instance_id)
                  )

                room =
                  update_in(
                    room.mobiles[mobile.ref].inventory,
                    &List.replace_at(&1, location, item)
                  )

                {:noreply, room} = RoomServer.handle_info(message, room)
                room
              else
                room
              end
            end)
          end)

        Enum.reduce(mobile.equipment, room, fn item, room ->
          item.timers
          |> Enum.reduce(room, fn {name, %{send_at: send_at, message: message}}, room ->
            if send_at < now do
              item = update_in(item.timers, &Map.delete(&1, name))

              location =
                Enum.find_index(
                  mobile.equipment,
                  &(&1.instance_id == item.instance_id)
                )

              room =
                update_in(
                  room.mobiles[mobile.ref].equipment,
                  &List.replace_at(&1, location, item)
                )

              {:noreply, room} = RoomServer.handle_info(message, room)
              room
            else
              room
            end
          end)
        end)

      _room, %{} = mobile ->
        mobile
    end)
  end

  def timers(%{timers: timers}) do
    Map.keys(timers)
  end

  def time_remaining(%{timers: timers}, name) do
    if timer = Map.get(timers, name) do
      timer.send_at - DateTime.to_unix(DateTime.utc_now(), :millisecond)
    else
      0
    end
  end

  def cancel(%{timers: timers} = entity, name) do
    put_in(entity.timers, Map.delete(timers, name))
  end

  def next_timer(%{timers: timers}) do
    timers
    |> Map.values()
    |> Enum.map(& &1.send_at)
    |> Enum.sort()
    |> List.first()
  end
end
