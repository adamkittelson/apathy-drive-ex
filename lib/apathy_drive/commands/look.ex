defmodule ApathyDrive.Commands.Look do
  require Logger
  use ApathyDrive.Command
  alias ApathyDrive.{Doors, Mobile, Match}

  @directions ["n", "north", "ne", "northeast", "e", "east",
              "se", "southeast", "s", "south", "sw", "southwest",
               "w", "west", "nw", "northwest", "u", "up", "d", "down"]

  def keywords, do: ["look", "l"]

  def execute(%Mobile{room_id: room_id} = mobile, args) do
    if blind?(mobile) do
      Mobile.send_scroll(mobile, "<p>You are blind.</p>")
    else
      room_id
      |> Room.find
      |> Room.look(%{mobile: self, name: Mobile.look_name(mobile), room_id: mobile.room_id}, args)
    end
  end

  def execute(%Room{id: id} = room, %{mobile: mobile, room_id: room_id, name: name}, []) when id != room_id do
    peek(room, name, room_id)
    execute(room, %{mobile: mobile}, [])
  end

  def execute(%Room{} = room, %{mobile: mobile}, []) do
    name_color =
      case room.room_unity && room.room_unity.unity do
        "demon" ->
          "magenta"
        "angel" ->
          "white"
        _ ->
          "cyan"
      end

    Mobile.send_scroll(mobile, "<p><span class='#{name_color}'>#{room.name}</span></p>")
    Mobile.send_scroll(mobile, "<p>    #{room.description}</p>")
    Mobile.send_scroll(mobile, "<p><span class='dark-cyan'>#{look_items(room)}</span></p>")
    Mobile.send_scroll(mobile, look_mobiles(room, mobile))
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>#{look_directions(room)}</span></p>")
    if room.light do
      Mobile.send_scroll(mobile, "<p>#{light_desc(room.light)}</p>")
    end
  end

  def execute(%Room{} = room, %{mobile: mobile} = mobile_data, arguments) when is_list(arguments) do
    cond do
      Enum.member?(@directions, Enum.join(arguments, " ")) ->
        room_exit = Room.get_exit(room, Enum.join(arguments, " "))
        execute(room, mobile_data, room_exit)
      target = find_mobile_in_room(room, mobile, Enum.join(arguments, " ")) ->
        Mobile.look_at_mobile(target.pid, %{name: mobile_data.name, looker: mobile})
      # target = mobile |> Mobile.room_id |> Room.find |> Room.find_item(Enum.join(arguments, " ")) ->
      #   look_at_item(mobile, target)
      # target = mobile |> Mobile.find_item(Enum.join(arguments, " ")) ->
      #   look_at_item(mobile, target)
      true ->
        Mobile.send_scroll(mobile, "<p>You do not notice that here.</p>")
    end
  end

  def execute(%Room{}, %{mobile: mobile}, nil) do
    Mobile.send_scroll(mobile, "<p>There is no exit in that direction!</p>")
  end

  def execute(%Room{}, %{mobile: mobile}, %{"kind" => kind}) when kind in ["RemoteAction", "Command"] do
    Mobile.send_scroll(mobile, "<p>There is no exit in that direction!</p>")
  end

  def execute(%Room{} = room, %{mobile: mobile} = mobile_data, %{"kind" => "Door"} = room_exit) do
    if Doors.open?(room, room_exit) do
      execute(room, mobile_data, Map.put(room_exit, "kind", "Normal"))
    else
      Mobile.send_scroll(mobile, "<p>The door is closed in that direction!</p>")
    end
  end

  def execute(%Room{} = room, %{mobile: mobile} = mobile_data, %{"kind" => "Hidden"} = room_exit) do
    if Doors.open?(room, room_exit) do
      execute(room, mobile_data, Map.put(room_exit, "kind", "Normal"))
    else
      Mobile.send_scroll(mobile, "<p>There is no exit in that direction!</p>")
    end
  end

  def execute(%Room{}, mobile_data, %{"destination" => destination}) do
    destination
    |> Room.find
    |> Room.look(mobile_data, [])
  end

  def peek(%Room{id: id} = room, name, room_id) do
    mirror_exit = Room.mirror_exit(room, room_id)

    if mirror_exit do
      message = "#{name} peeks in from #{Room.enter_direction(mirror_exit["direction"])}!"
                 |> capitalize_first

      ApathyDrive.Endpoint.broadcast! "rooms:#{id}:mobiles", "scroll", %{:html => "<p><span class='dark-magenta'>#{message}</span></p>"}
    end
  end

  def look_at_mobile(%Mobile{} = target, %{name: name, looker: mobile}) do
    Mobile.send_scroll(target, "<p>#{name} looks you over.</p>")

    hp_percentage = round(100 * (target.hp / target.max_hp))

    hp_description = case hp_percentage do
      _ when hp_percentage >= 100 ->
        "unwounded"
      _ when hp_percentage >= 90 ->
        "slightly wounded"
      _ when hp_percentage >= 60 ->
        "moderately wounded"
      _ when hp_percentage >= 40 ->
        "heavily wounded"
      _ when hp_percentage >= 20 ->
        "severely wounded"
      _ when hp_percentage >= 10 ->
        "critically wounded"
      _ ->
        "very critically wounded"
    end

    hp_description =
      "{{target:He/She/It}} appears to be #{hp_description}."
      |> interpolate(%{"target" => target})

    Mobile.send_scroll(mobile, "<p>#{Mobile.look_name(target)}</p>")
    Mobile.send_scroll(mobile, "<p>#{target.description}</p>")
    Mobile.send_scroll(mobile, "<p>#{hp_description}</p>")
  end

  def look_mobiles(%Room{also_here: mobiles}, mobile \\ nil) do
    mobiles_to_show =
      mobiles
      |> Enum.reduce([], fn({pid, %{look_name: name}}, list) ->
           if pid == mobile do
             list
           else
             [name | list]
           end
         end)

    if Enum.any?(mobiles_to_show) do
      "<p><span class='dark-magenta'>Also here:</span> #{Enum.join(mobiles_to_show, ", ")}<span class='dark-magenta'>.</span></p>"
    else
      ""
    end
  end

  def light_desc(light_level)  when light_level <= -100, do: "The room is barely visible"
  def light_desc(light_level)  when light_level <=  -25, do: "The room is dimly lit"
  def light_desc(_light_level), do: nil

  def display_direction(%{"kind" => "Gate", "direction" => direction} = room_exit, room) do
    case Doors.open?(room, room_exit) do
      true ->
        "open gate #{direction}"
      false ->
        "closed gate #{direction}"
    end
  end
  def display_direction(%{"kind" => "Door", "direction" => direction} = room_exit, room) do
    case Doors.open?(room, room_exit) do
      true ->
        "open door #{direction}"
      false ->
        "closed door #{direction}"
    end
  end
  def display_direction(%{"kind" => "Hidden", "description" => description} = room_exit, room) do
    if Doors.open?(room, room_exit), do: description
  end
  def display_direction(%{"kind" => kind}, _room) when kind in ["Command", "RemoteAction"], do: nil
  def display_direction(%{"direction" => direction}, _room), do: direction

  def exit_directions(%Room{} = room) do
    room.exits
    |> Enum.map(fn(room_exit) ->
        display_direction(room_exit, room)
       end)
    |> Enum.reject(&(&1 == nil))
  end

  def look_directions(%Room{} = room) do
    case exit_directions(room) do
      [] ->
        "Obvious exits: NONE"
      directions ->
        "Obvious exits: #{Enum.join(directions, ", ")}"
    end
  end

  def look_items(%Room{} = room) do
    psuedo_items = room.item_descriptions["visible"]
                   |> Map.keys

    items = case room.items do
      nil ->
        []
      items ->
        items
        |> Enum.map(&(&1["name"]))
    end

    items = items ++ psuedo_items

    case Enum.count(items) do
      0 ->
        ""
      _ ->
        "You notice #{Enum.join(items, ", ")} here."
    end
  end

  def blind?(%Mobile{} = mobile) do
    mobile.effects
    |> Map.values
    |> Enum.any?(&(Map.has_key?(&1, "blinded")))
  end

  defp find_mobile_in_room(%Room{also_here: mobiles}, mobile, string) do
    mobile =
      mobiles
      |> Map.values
      |> Enum.find(&(&1.pid == mobile))

    mobiles
    |> Map.values
    |> Enum.reject(&(&1 == mobile))
    |> List.insert_at(-1, mobile)
    |> Match.one(:name_contains, string)
  end
  #
  # defp look_at_item(mobile, description) when is_binary(description) do
  #   Mobile.send_scroll mobile, "<p>#{description}</p>"
  # end
  # defp look_at_item(mobile, %{} = item) do
  #   Mobile.look_at_item(mobile, item)
  # end

end
