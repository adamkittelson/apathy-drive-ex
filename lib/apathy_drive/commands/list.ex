defmodule Commands.List do
  use ApathyDrive.Command

  def keywords, do: ["list"]

  def execute(%Spirit{} = spirit, _arguments) do
    case Spirit.find_room(spirit) do
      %Room{shop_items: shop_items} = room when is_list(shop_items) ->
        Systems.Shop.list(spirit, nil, room)
      %Room{trainable_skills: skills} = room when is_list(skills) ->
        Systems.Trainer.list(spirit, nil, room)
      true ->
        send_message(spirit, "scroll", "<p><span class='red'>You cannot LIST if you are not in a shop!</span></p>")
    end
  end
end
