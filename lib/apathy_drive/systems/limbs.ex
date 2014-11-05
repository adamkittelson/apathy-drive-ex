defmodule Systems.Limbs do
  use Systems.Reload
  import Utility
  import Systems.Text

  def equipped_items(character) when is_pid(character) do
    character |> Components.Limbs.value |> equipped_items
  end

  def equipped_items(limbs) do
    Map.values(limbs) |> Enum.map(&(&1["items"])) |> List.flatten |> Enum.uniq
  end

  def equipped_weapons(monster) do
    monster
    |> equipped_items
    |> Enum.filter(fn(item) ->
         Components.Module.value(item).weapon?
       end)
  end

  def wielding_weapon?(entity) do
    entity
    |> equipped_weapons
    |> Enum.any?
  end

  def get_limb_names(limbs, item) do
    Map.keys(limbs) |> Enum.filter(fn(limb_name) ->
      limbs[limb_name]["items"] |> Enum.member?(item)
    end)
  end

  def cripple_limb(_entity, nil), do: nil

  def cripple_limb(entity, limb) do
    unless Components.Limbs.severed?(entity, limb) do
      limb_damage = Components.Limbs.current_damage(entity, limb)
      max_damage = Components.Limbs.max_damage(entity, limb)
      if limb_damage < max_damage do
        amt = Float.ceil((max_damage * 1.45) - limb_damage) |> trunc
        Components.Limbs.damage_limb(entity, limb, amt)
      end

      Parent.of(entity)
      |> Systems.Room.characters_in_room
      |> Enum.each(fn(character) ->

         observer = Possession.possessed(character) || character

         cond do
           observer == entity ->
             send_message(observer, "scroll", "<p>Your #{limb} is crippled!</p>")
            true ->
             send_message(observer, "scroll", "<p>#{capitalize_first(Components.Name.value(entity))}'s #{limb} is crippled!</p>")
         end
       end)
      cripple_limb(entity, Components.Limbs.attached(entity, limb))
    end
  end

  def sever_limb(_entity, nil), do: nil

  def sever_limb(entity, limb) do
    unless Components.Limbs.severed?(entity, limb) do
      Components.Limbs.sever_limb(entity, limb)
      Parent.of(entity)
      |> Systems.Room.characters_in_room
      |> Enum.each(fn(character) ->

         observer = Possession.possessed(character) || character

         cond do
           observer == entity ->
             if "torso" == limb do
               send_message(observer, "scroll", "<p>You've been dealt a mortal blow!</p>")
             else
               send_message(observer, "scroll", "<p>Your #{limb} has been severed!</p>")
             end
            true ->
              if "torso" == limb do
                send_message(observer, "scroll", "<p>#{capitalize_first(Components.Name.value(entity))} has been dealt a mortal blow!</p>")
              else
                send_message(observer, "scroll", "<p>#{capitalize_first(Components.Name.value(entity))}'s #{limb} has been severed!</p>")
              end
         end
       end)
      sever_limb(entity, Components.Limbs.attached(entity, limb))
    end
  end

end
