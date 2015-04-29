defmodule ApathyDrive.Exits.Hidden do
  use ApathyDrive.Exit

  def display_direction(room, room_exit) do
    if open?(room, room_exit) do
      room_exit["description"]
    end
  end

  def look(%Spirit{} = spirit, %Room{} = current_room, room_exit) do
    if open?(current_room, room_exit) or room_exit.passable_while_hidden do
      super(spirit, current_room, room_exit)
    else
      Phoenix.Channel.push spirit.socket, "scroll", %{:html => "<p>There is no exit in that direction!</p>"}
    end
  end

  def look(%Monster{} = monster, %Room{} = current_room, room_exit) do
    if open?(current_room, room_exit) or room_exit.passable_while_hidden do
      super(monster, current_room, room_exit)
    else
      ApathyDrive.Endpoint.broadcast! "monsters:#{monster.id}", "scroll", %{:html => "<p>There is no exit in that direction!</p>"}
    end
  end

  def search(monster, room, room_exit) do
    if searched?(room, room_exit) || :random.uniform(100) > monster.level do
      Monster.send_scroll(monster, "<p>You notice nothing different #{Exit.direction_description(room_exit["direction"])}.</p>")
    else
      if room_exit["message_when_revealed"] do
        Monster.send_scroll(monster, "<p>#{room_exit["message_when_revealed"]}</p>")
      end

      Systems.Effect.add(room, %{searched: room_exit["direction"]}, 300)
    end
  end

  def move(current_room, %Spirit{} = spirit, room_exit) do
    super(current_room, spirit, room_exit)
  end

  def move(current_room, %Monster{} = monster, room_exit) do
    if open?(current_room, room_exit) do
      super(current_room, monster, room_exit)
    else
      if monster do
        Monster.send_scroll(monster, "<p>There is no exit in that direction!</p>")
      end
    end
  end

  def open?(room, room_exit) do
    permanently_open?(room_exit) or
    all_remote_actions_triggered?(room_exit) or
    searched?(room, room_exit) or
    opened_remotely?(room, room_exit)
  end

  def permanently_open?(room_exit) do
    !!room_exit[:open]
  end

  def all_remote_actions_triggered?(room_exit) do
    if room_exit["remote_action_exits"] do
      room_exit["remote_action_exits"]
      |> Enum.all?(fn(remote_exit) ->
           Room.find(remote_exit["room"])
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

  def searched?(%Room{} = room, room_exit) do
    room.effects
    |> Map.values
    |> Enum.filter(fn(effect) ->
         Map.has_key?(effect, :searched)
       end)
    |> Enum.map(fn(effect) ->
         Map.get(effect, :searched)
       end)
    |> Enum.member?(room_exit["direction"])
  end

  def opened_remotely?(_room, _room_exit) do
    false
    #!!reactor.timer(self, :opened_remotely)
  end

end
