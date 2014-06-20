defmodule Systems.Login do
  use Systems.Reload

  def create do
    {:ok, character} = Entity.init
    Entity.add_component(character, Components.Experience, 0)
    Entity.add_component(character, Components.Level, 1)
    Entity.add_component(character, Components.Online, false)
    start_room_id = Components.find_by(Components.StartRoom, true) |> Components.ID.value
    Entity.add_component(character, Components.CurrentRoom, start_room_id)
    Entity.add_component(character, Components.Types, ["character"])
    Entity.add_component(character, Components.Items, [])
    Entity.add_component(character, Components.URL, Systems.URL.random)
    Entity.add_component(character, Components.Player, nil)
    Entity.add_component(character, Components.Spirit, true)
    Entity.add_component(character, Components.Skills, %{})
    Entity.add_component(character, Components.Idle, 0)
    Entity.add_component(character, Components.Hints, %Components.Hints{
      :active => %{
        "movement" => "To move from room to room simply type the direction in which you wish to travel. e.g. 'north' or 'south'. You may also abbreviate the directions e.g. 'nw' for 'northwest'."
      }
    })
    Entities.save!(character)
    Components.URL.value(character)
  end

  def login(player, url) do
    character = Characters.find_by_url(url)
    if character do
      Components.Online.value(character, true)
      Components.Player.value(character, player)
      Components.Idle.value(character, 0)
      Components.Player.send_message(character, ["clear scroll"])
      Systems.Room.display_room_in_scroll(character, Components.CurrentRoom.get_current_room(character))
      Systems.Command.display_prompt(character)
    else
      Players.send_message(player, ["redirect", "/"])
    end
  end
end
