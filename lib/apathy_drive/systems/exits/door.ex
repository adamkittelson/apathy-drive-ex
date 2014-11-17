defmodule Systems.Exits.Door do
  use Systems.Exit

  def name, do: "door"

  def display_direction(room, room_exit) do
    case open?(room, room_exit) do
      true ->
        "open #{name} #{room_exit["direction"]}"
      false ->
        "closed #{name} #{room_exit["direction"]}"
    end
  end

  def move(spirit, nil, current_room, room_exit) do
    destination = Rooms.find_by_id(room_exit["destination"])
    Components.Characters.remove_character(current_room, spirit)
    Components.Characters.add_character(destination, spirit)
    Entities.save!(destination)
    Entities.save!(current_room)
    Entities.save!(spirit)
    Components.Hints.deactivate(spirit, "movement")

    if !open?(current_room, room_exit) do
      send_message(spirit, "scroll", "<p><span class='dark-green'>You pass right through the door.</span></p>")
    end

    Systems.Room.display_room_in_scroll(spirit, destination)
  end

  def move(spirit, monster, current_room, room_exit) do
    if open?(current_room, room_exit) do
      super(spirit, monster, current_room, room_exit)
    else
      send_message(spirit, "scroll", "<p><span class='red'>The #{name} is closed!</span></p>")
    end
  end

  def open(monster, room, room_exit) do
    cond do
      locked?(room, room_exit) ->
        send_message(monster, "scroll", "<p>The #{name} is locked.</p>")
      open?(room, room_exit) ->
        send_message(monster, "scroll", "<p>The #{name} was already open.</p>")
      true ->
        open!(room, room_exit["direction"])
        send_message(monster, "scroll", "<p>The #{name} is now open.</p>")

        msg = "You see #{Components.Name.value(monster)} open the #{name} #{Exit.direction_description(room_exit["direction"])}."
        room
        |> Systems.Room.characters_in_room
        |> Enum.each(fn(character) ->
             observer = Possession.possessed(character) || character

             if observer != monster do
               send_message(observer, "scroll", "<p>#{msg}</p>")
             end
           end)

        #mirror_open(reactor)
    end
  end

  def open!(room, direction) do
    if Components.Exits.get_open_duration(room, direction) do
      Systems.Effect.add(room, %{open: direction}, Components.Exits.get_open_duration(room, direction))
    else
      Components.Exits.open_door(room, direction)
    end
  end

  def locked?(room, room_exit), do: false

  def open?(room, room_exit) do
    permanently_open?(room, room_exit) or
    all_remote_actions_triggered?(room, room_exit) or
    temporarily_open?(room, room_exit) or
    opened_remotely?(room, room_exit)
  end

  def permanently_open?(room, room_exit) do
    !!room_exit[:open]
  end

  def all_remote_actions_triggered?(room, room_exit) do
    false
    # if remote_action_exit_ids.present?
    #   remote_action_exit_ids.map do |exit_id|
    #     Exit.find(exit_id)
    #   end.all? do |exit|
    #     exit.remote_action_triggered?(reactor)
    #   end
    # end
  end

  def temporarily_open?(room, room_exit) do
    room
    |> Components.Effects.value
    |> Map.values
    |> Enum.filter(fn(effect) ->
         Map.has_key?(effect, :open)
       end)
    |> Enum.map(fn(effect) ->
         Map.get(effect, :open)
       end)
    |> Enum.member?(room_exit["direction"])
  end

  def opened_remotely?(room, room_exit) do
    false
    #!!reactor.timer(self, :opened_remotely)
  end

end
