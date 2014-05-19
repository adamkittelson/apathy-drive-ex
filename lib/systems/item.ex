defmodule Systems.Item do

  def spawn_item(item) do
    {:ok, entity} = ApathyDrive.Entity.init
    ApathyDrive.Entity.list_components(item)
    |> Enum.reject(&(&1 == Components.ID))
    |> Enum.each fn(component) ->
      ApathyDrive.Entity.add_component(entity, component, component.value(item))
    end
    ApathyDrive.Entity.save!(entity)
    entity
  end

  def spawn_item(item, entity) do
    item = spawn_item(item)
    Components.Items.add_item(entity, item)
    ApathyDrive.Entity.save!(entity)
  end

  def display_inventory(character) do
    items = Components.Items.get_items(character) |> Enum.map(&(Components.Name.value(&1))) |> Enum.join(", ")
    Components.Player.send_message(character, ["scroll", "<p>You are carrying #{items}</p>"])
  end

  def equip(character, item) do
    case Systems.Match.first(Components.Items.get_items(character), :name_contains, item) do
      nil ->
        Components.Player.send_message(character, ["scroll", "<p>You don't have \"#{item}\" left unequipped.</p>"])
      match ->
        case Components.Limbs.equip(character, match) do
          %{"removed" => item_removed} ->
            Components.Items.add_item(character, item_removed)
            Components.Player.send_message(character, ["scroll", "<p>You remove #{Components.Name.value(item_removed)}.</p>"])
          _ ->
        end
        Components.Items.remove_item(character, match)
        Components.Player.send_message(character, ["scroll", "<p>You are now wearing #{Components.Name.value(match)}.</p>"])
    end
  end
end
