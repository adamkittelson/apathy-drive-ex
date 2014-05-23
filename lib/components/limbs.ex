defmodule Components.Limbs do
  use GenEvent.Behaviour

  ### Public API
  def value(entity) do
    :gen_event.call(entity, Components.Limbs, :value)
  end

  def value(entity, new_value) do
    ApathyDrive.Entity.notify(entity, {:set_limbs, new_value})
  end

  def equip(entity, item) do
    :gen_event.call(entity, Components.Limbs, {:equip, item})
  end

  def serialize(entity) do
    %{"Limbs" => value(entity)}
  end


  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, value) do
    {:ok, value, value}
  end

  def handle_call({:equip, item}, value) do
    if ApathyDrive.Entity.has_component?(item, Components.WornOn) && ApathyDrive.Entity.has_component?(item, Components.Slot) do
      worn_on    = Components.WornOn.value(item)
      slot       = Components.Slot.value(item)
      open_limbs = Systems.Limbs.open_limbs(worn_on, value, slot)
      if Enum.count(open_limbs) < Enum.count(worn_on) do
        items_to_remove = Enum.filter(Systems.Limbs.equipped_items(value), fn(equipped_item) ->
          slot == Components.Slot.value(equipped_item)
        end)
        value = Enum.reduce(Map.keys(value), value, fn(limb_name, value) ->
          limb = value[limb_name]
          items = Enum.reduce(items_to_remove, limb["items"], fn(item_to_remove, items) ->
            List.delete(items, Components.ID.value(item_to_remove))
          end)
          limb = Map.put(limb, "items", items)
          Map.put(value, limb_name, limb)
        end)
        open_limbs = Systems.Limbs.open_limbs(worn_on, value, slot)
      end
      limbs_needed = Enum.count(worn_on)
      limbs_to_use = open_limbs |> Enum.take(limbs_needed)
      value = Systems.Limbs.equip_item(item, limbs_to_use, value)

      if items_to_remove do
        {:ok, %{"removed" => items_to_remove}, value}
      else
        {:ok, %{}, value}
      end
    else
      {:ok, %{"error" => "You can't wear that."}, value}
    end
  end

  def handle_event({:set_limbs, value}, _value) do
    {:ok, value}
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end