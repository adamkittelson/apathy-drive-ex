defmodule Systems.Item do

  def spawn_item(item) do
    {:ok, entity} = ApathyDrive.Entity.init
    ApathyDrive.Entity.list_components(item)
    |> Enum.reject(&(&1 == Components.ID))
    |> Enum.each fn(component) ->
      ApathyDrive.Entity.add_component(entity, component, component.value(item))
    end
    Components.Types.value(entity, ["item"])
    ApathyDrive.Entity.save!(entity)
    entity
  end

  def spawn_item(item, entity) do
    item = spawn_item(item)
    Components.Items.add_item(entity, item)
    ApathyDrive.Entity.save!(entity)
  end

  def display_inventory(character) do
    limbs = Components.Limbs.value(character)
    equipped_items = Components.Limbs.equipped_items(limbs)

    if equipped_items |> Enum.count > 0 do
      Components.Player.send_message(character, ["scroll", "<p><span class='dark-yellow'>You are equipped with:</span></p><br>"])
      equipped_items |> Enum.each fn(item) ->
        item_name = Components.Name.value(item)
        item_limbs = Components.Limbs.get_limb_names(limbs, item)
        Components.Player.send_message(character, ["scroll", "<p><span class='dark-green'>#{String.ljust(item_name, 20)}</span><span class='dark-cyan'>(#{Enum.join(item_limbs, ", ")})</span></p>"])
      end
    end
    items = Components.Items.get_items(character) |> Enum.map(&(Components.Name.value(&1)))
    if items |> Enum.count > 0 do
      Components.Player.send_message(character, ["scroll", "<br><p>You are carrying #{Enum.join(items, ", ")}</p>"])
    else
      Components.Player.send_message(character, ["scroll", "<br><p>You are carrying nothing.</p>"])
    end
  end

  def equip(character, item) do
    case Systems.Match.first(Components.Items.get_items(character), :name_contains, item) do
      nil ->
        Components.Player.send_message(character, ["scroll", "<p>You don't have \"#{item}\" left unequipped.</p>"])
      match ->
        case Components.Limbs.equip(character, match) do
          %{"removed" => items_removed} ->
            Enum.each(items_removed, fn(item_removed) ->
              Components.Items.add_item(character, item_removed)
              Components.Player.send_message(character, ["scroll", "<p>You remove #{Components.Name.value(item_removed)}.</p>"])
            end)
            Components.Items.remove_item(character, match)
            Components.Player.send_message(character, ["scroll", "<p>You are now wearing #{Components.Name.value(match)}.</p>"])
          %{"error" => message} ->
            Components.Player.send_message(character, ["scroll", "<p>#{message}</p>"])
          _ ->
            Components.Items.remove_item(character, match)
            Components.Player.send_message(character, ["scroll", "<p>You are now wearing #{Components.Name.value(match)}.</p>"])
        end
    end
  end
end
