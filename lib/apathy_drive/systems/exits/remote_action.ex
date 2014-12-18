defmodule Systems.Exits.RemoteAction do
  use Systems.Exit

  def display_direction(_room, room_exit), do: nil

  def move(spirit, monster, current_room, room_exit) do
    send_message(spirit, "scroll", "<p>There is no exit in that direction.</p>")
  end

  def look(spirit, monster, current_room, room_exit) do
    send_message(spirit, "scroll", "<p>There is no exit in that direction.</p>")
  end

  def trigger_remote_action(spirit, monster, room, room_exit) do
    remote_room = Rooms.find_by_id(room_exit["destination"])
    remote_exit = Components.Exits.value(remote_room)
                  |> Enum.find(fn(remote_exit) ->
                       remote_exit["remote_action_exits"] && Enum.member?(remote_exit["remote_action_exits"],
                                                                          %{"room" => Components.ID.value(room),
                                                                            "direction" => room_exit["direction"]})
                     end)

    if trigger_remote_action?(remote_room, remote_exit, room_exit) do
      Systems.Effect.add(room, %{triggered: room_exit["direction"]}, 300)

      send_message(monster, "scroll", "<p>#{room_exit["message"]}</p>")
      Systems.Monster.observers(room, monster)
      |> Enum.each(fn(observer) ->
        send_message(observer, "scroll", "<p><span class='dark-green'>#{interpolate(room_exit["room_message"], %{"user" => monster})}</span></p>")
      end)

      if :"Elixir.Systems.Exits.#{remote_exit["kind"]}".open?(remote_room, remote_exit) do
        if remote_exit["message_when_revealed"] do
          Systems.Monster.observers(remote_room, nil)
          |> Enum.each(fn(observer) ->
            send_message(observer, "scroll", "<p><span class='white'>#{remote_exit["message_when_revealed"]}</span></p>")
          end)
        end
      end
    else
      clear_triggers!(remote_exit)
      trigger_remote_action(spirit, monster, room, room_exit)
    end
  end

  def trigger_remote_action?(remote_room, %{"remote_action_order_matters" => true} = remote_exit, room_exit) do
    remote_exit["remote_action_exits"]
    |> Enum.filter(&triggered?/1)
    |> Enum.all?(fn(ra_exit) ->
         remote_action_order = ra_exit["room"]
                             |> Rooms.find_by_id
                             |> Components.Exits.value
                             |> Enum.find(fn(other_remote_exit) ->
                                            other_remote_exit["direction"] == ra_exit["direction"]
                                          end)
                             |> Map.get("remote_action_order")

         remote_action_order == nil or remote_action_order < room_exit["remote_action_order"]
       end)
  end
  def trigger_remote_action?(_remote_room, _remote_exit, _room_exit), do: true

  def clear_triggers!(remote_exit) do
    remote_exit["remote_action_exits"]
    |> Enum.each(fn(map) ->
         room = map["room"]
                |> Rooms.find_by_id

         effects = room
                   |> Components.Effects.value

         effects
         |> Map.keys
         |> Enum.filter(fn(key) ->
              effects[key][:triggered] == map["direction"]
            end)
         |> Enum.each(&(Components.Effects.remove(room, &1)))
       end)
  end

  def triggered?(room_direction) do
    room = room_direction["room"]
           |> Rooms.find_by_id

    effects = room
              |> Components.Effects.value

    effects
    |> Map.keys
    |> Enum.any?(fn(key) ->
         effects[key][:triggered] == room_direction["direction"]
       end)
  end

  # [%{"commands" => ["push knot"], "destination" => 23579,
  #    "direction" => "east", "kind" => "RemoteAction",
  #    "message" => "You push a large knot on the darkwood tree.",
  #    "remote_action_order" => 1,
  #    "room_message" => "{{User}} pushes a large knot on the darkwood tree."},
  #  %{"commands" => ["twist knot", "turn knot"], "destination" => 23579,
  #    "direction" => "west", "kind" => "RemoteAction",
  #    "message" => "You twist a large knot on the darkwood tree.",
  #    "remote_action_order" => 2,
  #    "room_message" => "{{User}} twists a large knot on the darkwood tree."},
  #  %{"destination" => 4493, "direction" => "northeast",
  #    "kind" => "Normal"},
  #  %{"description" => "crack in tree southwest", "destination" => 23583,
  #    "direction" => "southwest", "kind" => "Hidden",
  #    "message_when_revealed" => "A groaning crack in the darkwood tree yawns open!",
  #    "passable_while_hidden" => false,
  #    "remote_action_exits" => [%{"direction" => "east", "room" => 23579},
  #     %{"direction" => "west", "room" => 23579}],
  #    "remote_action_order_matters" => true, "searchable" => false}]
end