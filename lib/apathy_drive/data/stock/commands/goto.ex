defmodule Commands.Goto do
  use Systems.Command

  def keywords, do: ["goto"]


  def execute(spirit, nil, arguments) do
    current_room = Parent.of(spirit)

    destination = arguments
                  |> Enum.join
                  |> String.to_integer
                  |> Rooms.find_by_id

    Components.Characters.remove_character(current_room, spirit)
    Components.Characters.add_character(destination, spirit)
    Entities.save!(destination)
    Entities.save!(current_room)
    Entities.save!(spirit)
    Systems.Room.display_room_in_scroll(spirit, destination)
  end

  def execute(spirit, monster, arguments) do
    current_room = Parent.of(spirit)

    destination = arguments
                  |> Enum.join
                  |> String.to_integer
                  |> Rooms.find_by_id

    Components.Monsters.remove_monster(current_room, monster)
    Components.Monsters.add_monster(destination, monster)
    Components.Characters.remove_character(current_room, spirit)
    Components.Characters.add_character(destination, spirit)
    Entities.save!(destination)
    Entities.save!(current_room)
    Entities.save!(spirit)
    Entities.save(monster)
    Systems.Room.display_room_in_scroll(monster, destination)
  end

end
