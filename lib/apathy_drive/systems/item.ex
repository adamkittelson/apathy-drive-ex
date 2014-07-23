defmodule Systems.Item do
  use Systems.Reload
  import Utility

  def spawn_item(item) do
    {:ok, entity} = Entity.init
    Entity.list_components(item)
    |> Enum.reject(&(&1 == Components.ID))
    |> Enum.each fn(component) ->
      Entity.add_component(entity, component, component.value(item))
    end

    template = Components.Module.value(item)

    if template.properties[:worn_on] do
      Entity.add_component(entity, Components.WornOn, template.properties[:worn_on])
    end

    if template.properties[:slot] do
      Entity.add_component(entity, Components.Slot, template.properties[:slot])
    end

    if template.properties[:ac] do
      Entity.add_component(entity, Components.AC, template.properties[:ac] || 0)
    end

    Entity.add_component(entity, Components.Module, template)

    Entity.add_component(entity, Components.Types, ["item"])
    Entities.save!(entity)
    entity
  end

  def spawn_item(item, entity) do
    item = spawn_item(item)
    Components.Items.add_item(entity, item)
    Entities.save!(entity)
  end

  def display_inventory(character) do
    if Entity.has_component?(character, Components.Limbs) do
      limbs = Components.Limbs.value(character)
      equipped_items = Systems.Limbs.equipped_items(limbs)

      if equipped_items |> Enum.count > 0 do
        send_message(character, "scroll", "<p><span class='dark-yellow'>You are equipped with:</span></p><br>")
        equipped_items |> Enum.each fn(item) ->
          item_name = Components.Name.value(item)
          item_limbs = Systems.Limbs.get_limb_names(limbs, item)
          send_message(character, "scroll", "<p><span class='dark-green'>#{String.ljust(item_name, 20)}</span><span class='dark-cyan'>(#{Enum.join(item_limbs, ", ")})</span></p>")
        end
      end
    end

    items = Components.Items.get_items(character) |> Enum.map(&(Components.Name.value(&1)))
    if items |> Enum.count > 0 do
      send_message(character, "scroll", "<br><p>You are carrying #{Enum.join(items, ", ")}</p>")
    else
      send_message(character, "scroll", "<br><p>You are carrying nothing.</p>")
    end
  end

  def equip(character, item) do
    case Systems.Match.first(Components.Items.get_items(character), :name_contains, item) do
      nil ->
        send_message(character, "scroll", "<p>You don't have \"#{item}\" left unequipped.</p>")
      match ->
        case Components.Limbs.equip(character, match) do
          %{"removed" => items_removed} ->
            Enum.each(items_removed, fn(item_removed) ->
              Components.Items.add_item(character, item_removed)
              send_message(character, "scroll", "<p>You remove #{Components.Name.value(item_removed)}.</p>")
            end)
            Components.Items.remove_item(character, match)
            send_message(character, "scroll", "<p>You are now wearing #{Components.Name.value(match)}.</p>")
            Components.Attacks.reset_attacks(character)
            Entities.save!(character)
          %{"error" => message} ->
            send_message(character, "scroll", "<p>#{message}</p>")
          _ ->
            Components.Items.remove_item(character, match)
            send_message(character, "scroll", "<p>You are now wearing #{Components.Name.value(match)}.</p>")
            Components.Attacks.reset_attacks(character)
            Entities.save!(character)
        end
    end
  end

  def unequip(character, item) do
    case Systems.Match.first(Systems.Limbs.equipped_items(character), :name_contains, item) do
      nil ->
        send_message(character, "scroll", "<p>You are not wearing \"#{item}\".</p>")
      match ->
        Components.Limbs.unequip(character, match)
        Components.Items.add_item(character, match)
        send_message(character, "scroll", "<p>You remove #{Components.Name.value(match)}.</p>")
        Components.Attacks.reset_attacks(character)
        Entities.save!(character)
    end
  end
end
