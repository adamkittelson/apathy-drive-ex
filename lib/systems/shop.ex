defmodule Systems.Shop do
  def list(character, room) do
    if ApathyDrive.Entity.has_component?(room, Components.Shop) do
      Components.Player.send_message(character, ["scroll", "<p><span class='dark-green'>Item</span>                          <span class='dark-cyan'>Price</span></p>"])
      Components.Player.send_message(character, ["scroll", "<p><span class='dark-cyan'>─────────────────────────────────────────────────────────────────</span></p>"])
      Enum.each(Components.Shop.value(room), fn(item_hash) ->
        item_name = Components.find_by(Components.ID, item_hash["item"]) |> Components.Name.value
        cost = case item_hash["cost"] do
          nil ->
            "Free"
          amount when amount == 1 ->
            "#{amount} #{item_hash["denomination"]}"
          amount ->
            "#{amount} #{item_hash["denomination"]}s"
        end
        Components.Player.send_message(character, ["scroll", "<p><span class='dark-green'>#{String.ljust(item_name, 30)}</span><span class='dark-cyan'>#{cost}</span></p>"])
      end)
    else
      Components.Player.send_message(character, ["scroll", "<p><span class='red'>You cannot LIST if you are not in a shop!</span></p>"])
    end
  end
end