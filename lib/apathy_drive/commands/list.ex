defmodule Commands.List do
  use ApathyDrive.Command

  def keywords, do: ["list"]

  def execute(%Spirit{} = spirit, _arguments) do
    case Spirit.find_room(spirit) do
      %Room{shop_items: shop_items} = room when is_list(shop_items) ->
        Systems.Shop.list(spirit, room)
      %Room{trainable_skills: skills} = room when is_list(skills) ->
        Systems.Trainer.list(spirit, room)
      _ ->
        Spirit.send_scroll(spirit, "<p><span class='red'>You cannot LIST if you are not in a shop!</span></p>")
    end
  end

  def execute(%Monster{} = monster, _arguments) do
    case Monster.find_room(monster) do
      %Room{shop_items: shop_items} = room when is_list(shop_items) ->
        Systems.Shop.list(monster, room)
      %Room{trainable_skills: skills} = room when is_list(skills) ->
        Systems.Trainer.list(monster, room)
      _ ->
        Monster.send_scroll(monster, "<p><span class='red'>You cannot LIST if you are not in a shop!</span></p>")
    end
  end
end
