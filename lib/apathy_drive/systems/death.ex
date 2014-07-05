defmodule Systems.Death do
  use Systems.Reload
  import Utility
  import Systems.Text

  def kill(entity) do
    entity
    |> Components.CurrentRoom.get_current_room
    |> Systems.Room.characters_in_room
    |> Enum.each(fn(character) ->
         if character == entity do
           send_message(character, "scroll", "<p><span class='red'>You have been killed!</span></p>")
         else

           send_message(character, "scroll", "<p>#{death_message(entity)}</p>")
         end
       end)
  end

  def death_message(entity) do
    default = "#{capitalize_first(Components.Name.value(entity))} drops <span class='dark-red'>dead</span> before you."
    if Entity.has_component?(entity, Components.Module) do
      Components.Module.value(entity).properties[:death_message] || default
    else
      default
    end
  end
end