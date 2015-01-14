defmodule Systems.Login do
  use Systems.Reload
  import Utility

  def create do
    url = Systems.URL.random

    Spirit.create(url)

    url
  end

  def login(socket, url) do
    spirit = Spirit.find_by_url(url)
    if spirit do
      spirit = Map.put(spirit, :socket, socket)

      Spirits.remove(spirit)
      spirit = Spirits.add(spirit)

      if !Parent.of(spirit) do

        room = Spirit.value(spirit).room_id
               |> Rooms.find_by_id

        Room.add_spirit(room, spirit)
      end
      # Components.Hints.add(spirit, "movement", "To move from room to room simply type the direction in which you wish to travel. e.g. 'north' or 'south'. You may also abbreviate the directions e.g. 'nw' for 'northwest'.")
      # Components.Hints.add(spirit, "name", "Many actions, such as communicating with other players or taking on a physical form, require you to name yourself. To choose the name by which you will be known in-game type 'set name (name)'.")
      Possession.unpossess(spirit)
      spirit
    end
  end

  defp move_character_to_start_room(spirit) do
    start_room = Room.start_room_id
                 |> Rooms.find_by_id
    Room.add_spirit(start_room, spirit)
  end
end
