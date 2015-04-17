defmodule ApathyDrive.Exits.Cast do
  use ApathyDrive.Exit

  def move(current_room, %Spirit{} = spirit, room_exit),  do: super(current_room, spirit, room_exit)

  def move(current_room, %Monster{} = monster, room_exit) do
    if room_exit["pre_exit_ability"] do
      case Abilities.find(room_exit["pre_exit_ability"]) do
        nil ->
          Monster.send_scroll(monster, "<p><span class='red'>Not Implemented: #{room_exit["pre_exit_ability"]}</span></p>")
        ability ->
          ability.execute(monster, nil)
      end
    end

    destination = Room.find(room_exit["destination"])
    Monster.send_scroll(monster, "<p><span class='yellow'>#{interpolate(room_exit["mover_message"], %{"user" => monster})}</span></p>")

    if room_exit["from_message"] do
      Systems.Monster.observers(current_room, monster)
      |> Enum.each(fn(observer) ->
        Monster.send_scroll(observer, "<p><span class='dark-green'>#{interpolate(room_exit["from_message"], %{"user" => monster})}</span></p>")
      end)
    else
      Systems.Monster.display_exit_message(current_room, monster)
    end

    if room_exit["post_exit_ability"] do
      case Abilities.find(room_exit["post_exit_ability"]) do
        nil ->
          Monster.send_scroll(monster, "<p><span class='red'>Not Implemented: #{room_exit["post_exit_ability"]}</span></p>")
        ability ->
          ability.execute(monster, nil)
      end
    end

    if room_exit["to_message"] do
      Systems.Monster.observers(destination, monster)
      |> Enum.each(fn(observer) ->
        Monster.send_scroll(observer, "<p><span class='dark-green'>#{interpolate(room_exit["to_message"], %{"user" => monster})}</span></p>")
      end)
    else
      Systems.Monster.display_enter_message(destination, monster)
    end

    Systems.Aggression.monster_entered(monster, destination)

    #Spirit.deactivate_hint(spirit, "movement")
    #Systems.Room.display_room_in_scroll(spirit, monster, destination)
    Monster.pursue(current_room, monster, room_exit["direction"])
  end

  def look(%Spirit{} = spirit, %Room{} = current_room, room_exit) do
    if room_exit.look_message do
      Phoenix.Channel.push spirit.socket, "scroll", %{:html => "<p>#{room_exit.look_message}</p>"}
    else
      super(spirit, current_room, room_exit)
    end
  end

  def look(%Monster{} = monster, %Room{} = current_room, room_exit) do
    if room_exit.look_message do
      ApathyDrive.Endpoint.broadcast! "monsters:#{monster.id}", "scroll", %{:html => "<p>#{room_exit.look_message}</p>"}
    else
      super(monster, current_room, room_exit)
    end
  end

end
