defmodule ApathyDrive.Doors do
  alias ApathyDrive.Room

  def name, do: "door"

  def locked?(room, room_exit) do
    !open?(room, room_exit) and !Room.unlocked?(room, room_exit["direction"])
  end

  def open?(room, room_exit) do
    permanently_open?(room_exit) or
    all_remote_actions_triggered?(room, room_exit) or
    Room.temporarily_open?(room, room_exit["direction"]) or
    Room.searched?(room, room_exit["direction"]) or
    opened_remotely?(room, room_exit)
  end

  def permanently_open?(room_exit) do
    !!room_exit["open"]
  end

  def all_remote_actions_triggered?(room, room_exit) do
    if room_exit["remote_action_exits"] do
      room_exit["remote_action_exits"]
      |> Enum.all?(fn(remote_exit) ->
           triggered?(room, remote_exit)
         end)
    else
      false
    end
  end

  def triggered?(room, %{} = room_exit) do
    room
    |> Map.get(:effects)
    |> Map.values
    |> Enum.filter(fn(effect) ->
         Map.has_key?(effect, :triggered)
       end)
    |> Enum.map(fn(effect) ->
          effect.triggered.remote_exit
       end)
    |> Enum.member?(room_exit)
  end

  def triggered?(room, direction) do
    room
    |> Map.get(:effects)
    |> Map.values
    |> Enum.filter(fn(effect) ->
         Map.has_key?(effect, :triggered)
       end)
    |> Enum.filter(fn(effect) ->
          effect.triggered.direction == direction
       end)
  end

  def opened_remotely?(room, %{"direction" => direction}) do
    room
    |> Map.get(:effects)
    |> Map.values
    |> Enum.filter(fn(effect) ->
         Map.has_key?(effect, :opened_remotely)
       end)
    |> Enum.map(fn(effect) ->
          effect.opened_remotely
       end)
    |> Enum.member?(direction)
  end

end
