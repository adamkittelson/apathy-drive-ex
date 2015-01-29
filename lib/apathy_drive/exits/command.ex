defmodule ApathyDrive.Exits.Command do
  use ApathyDrive.Exit

  def display_direction(_room, room_exit) do
    room_exit[:name]
  end

  def move(_current_room, %Spirit{} = spirit, _room_exit)  do
    Spirit.send_scroll(spirit, "<p>There is no exit in that direction.</p>")
  end

  #unpossessed monster
  def move(current_room, %Monster{} = monster, room_exit) do
    move_via_command(nil, monster, current_room, room_exit)
  end

  #posssessed monster
  def move(current_room, %Monster{} = monster, room_exit) do
    send_message(monster, "scroll", "<p>There is no exit in that direction.</p>")
  end

  def move_via_command(current_room, %Spirit{} = spirit, room_exit) do
    Spirit.send_scroll(spirit, "<p><span class='yellow'>#{room_exit.mover_message}</span></p>")

    new_room = Room.find(room_exit.destination)
               |> Room.value

    Room.look(new_room, spirit)

    spirit
    |> Spirit.set_room_id(room_exit.destination)
    |> Spirit.deactivate_hint("movement")
    |> Spirit.save
  end

  def move_via_command(nil, monster, current_room, room_exit) do
    if !Systems.Combat.stunned?(monster) do
      destination = Room.find(room_exit["destination"])
      Components.Monsters.remove_monster(current_room, monster)
      Components.Monsters.add_monster(destination, monster)
      if Entity.has_component?(monster, Components.ID) do
        Entities.save!(destination)
        Entities.save!(current_room)
      end
      Entities.save(monster)

      if room_exit["from_message"] do
        Systems.Monster.observers(current_room, monster)
        |> Enum.each(fn(observer) ->
          send_message(observer, "scroll", "<p><span class='dark-green'>#{interpolate(room_exit["from_message"], %{"user" => monster})}</span></p>")
        end)
      else
        Systems.Monster.display_exit_message(current_room, monster)
      end

      if room_exit["to_message"] do
        Systems.Monster.observers(destination, monster)
        |> Enum.each(fn(observer) ->
          send_message(observer, "scroll", "<p><span class='dark-green'>#{interpolate(room_exit["to_message"], %{"user" => monster})}</span></p>")
        end)
      else
        Systems.Monster.display_enter_message(destination, monster)
      end

      Systems.Aggression.monster_entered(monster, destination)
    end
  end

  def move_via_command(spirit, monster, current_room, room_exit) do
    if Systems.Combat.stunned?(monster) do
      send_message(monster, "scroll", "<p><span class='yellow'>You are stunned and cannot move!</span></p>")
    else

      destination = Room.find(room_exit["destination"])
      Components.Monsters.remove_monster(current_room, monster)
      Components.Monsters.add_monster(destination, monster)
      Components.Characters.remove_character(current_room, spirit)
      Components.Characters.add_character(destination, spirit)
      Entities.save!(destination)
      Entities.save!(current_room)
      send_message(monster, "scroll", "<p><span class='yellow'>#{interpolate(room_exit["mover_message"], %{"user" => monster})}</span></p>")
      Entities.save!(spirit)
      Entities.save(monster)

      if room_exit["from_message"] do
        Systems.Monster.observers(current_room, monster)
        |> Enum.each(fn(observer) ->
          send_message(observer, "scroll", "<p><span class='dark-green'>#{interpolate(room_exit["from_message"], %{"user" => monster})}</span></p>")
        end)
      else
        Systems.Monster.display_exit_message(current_room, monster)
      end

      if room_exit["to_message"] do
        Systems.Monster.observers(destination, monster)
        |> Enum.each(fn(observer) ->
          send_message(observer, "scroll", "<p><span class='dark-green'>#{interpolate(room_exit["to_message"], %{"user" => monster})}</span></p>")
        end)
      else
        Systems.Monster.display_enter_message(destination, monster)
      end

      Systems.Aggression.monster_entered(monster, destination)

      Spirit.deactivate_hint(spirit, "movement")
      Systems.Room.display_room_in_scroll(spirit, monster, destination)
    end
  end

  def look(%Spirit{} = spirit, %Room{} = current_room, room_exit) do
    Phoenix.Channel.reply spirit.socket, "scroll", %{:html => "<p>There is no exit in that direction.</p>"}
  end

  def look(%Monster{} = monster, %Room{} = current_room, room_exit) do
    Phoenix.Channel.broadcast "monsters:#{monster.id}", "scroll", %{:html => "<p>There is no exit in that direction.</p>"}
  end

end
