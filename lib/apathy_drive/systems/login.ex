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

      spirit = Spirit.login(spirit)

      if !Parent.of(spirit) do

        room = Spirit.value(spirit).room_id
               |> Rooms.find_by_id

        Room.add_spirit(room, spirit)
      end

      Spirit.activate_hint(spirit, "movement")
      Spirit.activate_hint(spirit, "name")
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
