defmodule Systems.Exits.Hidden do
  use Systems.Exit

  def display_direction(room, room_exit) do
    if open?(room, room_exit) do
      room_exit["description"]
    end
  end

  def look(spirit, monster, current_room, room_exit) do
    if open?(current_room, room_exit) or room_exit["passable_while_hidden"] do
      super(spirit, monster, current_room, room_exit)
    else
      send_message(spirit, "scroll", "<p>There is no exit in that direction!</p>")
    end
  end

  def search(monster, room, room_exit) do
    if searched?(room, room_exit) || :random.uniform(100) > Skills.Perception.modified(monster) do
      send_message(monster, "scroll", "<p>You notice nothing different #{Exit.direction_description(room_exit["direction"])}.</p>")
    else
      if room_exit["message_when_revealed"] do
        send_message(monster, "scroll", "<p>#{room_exit["message_when_revealed"]}</p>")
      end

      Systems.Effect.add(room, %{searched: room_exit["direction"]}, 300)
    end
  end

  def move(spirit, nil, current_room, room_exit) do
    super(spirit, nil, current_room, room_exit)
  end

  def move(spirit, monster, current_room, room_exit) do
    if open?(current_room, room_exit) do
      super(spirit, monster, current_room, room_exit)
    else
      if spirit do
        send_message(spirit, "scroll", "<p>There is no exit in that direction!</p>")
      end
    end
  end

  def open?(room, room_exit) do
    permanently_open?(room, room_exit) or
    all_remote_actions_triggered?(room, room_exit) or
    searched?(room, room_exit) or
    opened_remotely?(room, room_exit)
  end

  def permanently_open?(room, room_exit) do
    !!room_exit[:open]
  end

  def all_remote_actions_triggered?(room, room_exit) do
    if room_exit["remote_action_exits"] do
      room_exit["remote_action_exits"]
      |> Enum.all?(fn(remote_exit) ->
           Rooms.find_by_id(remote_exit["room"])
           |> Components.Effects.value
           |> Map.values
           |> Enum.filter(fn(effect) ->
                Map.has_key?(effect, :triggered)
              end)
           |> Enum.map(fn(effect) ->
                Map.get(effect, :triggered)
              end)
           |> Enum.member?(remote_exit["direction"])
         end)
    else
      false
    end
  end

  def searched?(room, room_exit) do
    room
    |> Components.Effects.value
    |> Map.values
    |> Enum.filter(fn(effect) ->
         Map.has_key?(effect, :searched)
       end)
    |> Enum.map(fn(effect) ->
         Map.get(effect, :searched)
       end)
    |> Enum.member?(room_exit["direction"])
  end

  def opened_remotely?(room, room_exit) do
    false
    #!!reactor.timer(self, :opened_remotely)
  end

end
