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
           if !Components.Spirit.value(character) do
             reward_player(character, entity)
           end
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

  def experience_to_grant(entity) when is_pid entity do
    Systems.Stat.modified(entity)
    |> Map.values
    |> Enum.sum
    |> experience_to_grant
  end

  def experience_to_grant(stat_total) do
    Float.floor(stat_total * (1 + (stat_total * 0.005)))
  end

  def reward_player(entity, deceased) do
    exp = experience_to_grant(deceased)
    Components.Experience.add(entity, exp)
    send_message(entity, "scroll", "<p>You gain #{exp} experience.")
  end
end
