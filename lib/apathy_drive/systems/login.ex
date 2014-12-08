defmodule Systems.Login do
  use Systems.Reload
  import Utility

  def create do
    {:ok, character} = Entity.init
    Entity.add_component(character, Components.Experience, 0)
    Entity.add_component(character, Components.Level, 1)
    Entity.add_component(character, Components.Online, false)
    Entity.add_component(character, Components.Types, ["character"])
    url = Systems.URL.random
    Entity.add_component(character, Components.URL, url)
    Entity.add_component(character, Components.Socket, nil)
    Entity.add_component(character, Components.Spirit, true)
    Entity.add_component(character, Components.Skills, %{})
    Entity.add_component(character, Components.Idle, 0)
    Entity.add_component(character, Components.Hints, %{"active" => %{}, "inactive" => []})
    Characters.add(character, url: url)
    Entities.save!(character)
    Characters.add(character, id: Components.ID.value(character))
    move_character_to_start_room(character)

    Components.URL.value(character)
  end

  def login(socket, url) do
    character = Characters.find_by_url(url)
    if character do
      Characters.add(character, socket: socket.pid)
      if !Parent.of(character) do
        move_character_to_start_room(character)
      end
      Components.Hints.add(character, "movement", "To move from room to room simply type the direction in which you wish to travel. e.g. 'north' or 'south'. You may also abbreviate the directions e.g. 'nw' for 'northwest'.")
      Components.Hints.add(character, "name", "Many actions, such as communicating with other players or taking on a physical form, require you to name yourself. To choose the name by which you will be known in-game type 'set name (name)'.")
      Components.Online.value(character, true)
      Components.Socket.value(character, socket)
      Components.Idle.value(character, 0)
      Possession.unpossess(character)
      send_message(character, "clear scroll")
      Systems.Room.display_room_in_scroll(character, Parent.of(character))
      Systems.Prompt.display(character, nil)
    else
      Phoenix.Channel.reply socket, "redirect", %{:url => "/"}
    end
  end

  defp move_character_to_start_room(character) do
    start_room = ApathyDrive.Config.get(:start_room)
                 |> Rooms.find_by_id
    Components.Characters.add_character(start_room, character)
  end
end
