defmodule Systems.Limbs do
  use Systems.Reload
  import Utility
  import Systems.Text
  use Timex

  def equipped_items(character) when is_pid(character) do
    character |> Components.Limbs.get_items
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
      items = limbs[limb_name]["items"]
      "#{inspect items} - #{inspect item}"
      items |> Enum.member?(item)
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
      room = Parent.of(entity)

      room
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

        {:ok, corpse} = Entity.init
        Entity.add_component(corpse, Components.Name,        "the #{limb} of #{Components.Name.value(entity)}")
        Entity.add_component(corpse, Components.Description, "This is the severed #{limb} of #{Components.Name.value(entity)}.")
        Entity.add_component(corpse, Components.Keywords, String.split(limb))
        Entity.add_component(corpse, Components.Module, Items.SeveredLimb)
        Entity.add_component(corpse, Components.Types, ["item", "corpse"])
        Entity.add_component(corpse, Components.Decay, %{"frequency" => 1, "decay_at" => Date.convert(Date.shift(Date.now, mins: 5), :secs)})
        Entities.save!(corpse)

        Components.Items.add_item(room, corpse)

        Entities.save!(room)

      sever_limb(entity, Components.Limbs.attached(entity, limb))
    end
  end

end
