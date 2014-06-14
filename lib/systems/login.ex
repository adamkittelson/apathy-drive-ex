defmodule Systems.Login do
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
    Entities.save!(character)
    Components.URL.value(character)
  end
end
