defmodule Systems.Login do
  use Systems.Reload
  import Utility

  def create do
    {:ok, character} = Entity.init
    Entity.add_component(character, Components.Experience, 0)
    Entity.add_component(character, Components.Level, 1)
    Entity.add_component(character, Components.Online, false)
    Entity.add_component(character, Components.Types, ["character"])
    Entity.add_component(character, Components.Items, [])
    Entity.add_component(character, Components.URL, Systems.URL.random)
    Entity.add_component(character, Components.Socket, nil)
    Entity.add_component(character, Components.Spirit, true)
    Entity.add_component(character, Components.Skills, %{})
    Entity.add_component(character, Components.Idle, 0)
    Entity.add_component(character, Components.Hints, %{})
    Entities.save!(character)
    move_character_to_start_room(character)

    Components.URL.value(character)
  end

  def login(socket, url) do
    character = Characters.find_by_url(url)
    if character do
      if !Parent.of(character) do
        move_character_to_start_room(character)
      end
      Components.Hints.add(character, "movement", "To move from room to room simply type the direction in which you wish to travel. e.g. 'north' or 'south'. You may also abbreviate the directions e.g. 'nw' for 'northwest'.")
      Components.Hints.add(character, "name", "Many actions, such as communicating with other players or taking on a physical form, require you to name yourself. To choose the name by which you will be known in-game type 'set name (name)'.")
      Components.Online.value(character, true)
      Components.Socket.value(character, socket)
      Components.Idle.value(character, 0)
      send_message(character, "clear scroll")
      Systems.Room.display_room_in_scroll(character, Parent.of(character))
      Systems.Command.display_prompt(character)
    else
      Phoenix.Channel.reply socket, "redirect", "/"
    end
  end

  defp move_character_to_start_room(character) do
    start_room = Components.find_by(Components.StartRoom, true)
    Components.Characters.add_character(start_room, character)
  end
end
