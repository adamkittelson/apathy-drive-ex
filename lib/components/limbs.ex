defmodule Components.Limbs do
  use GenEvent.Behaviour

  ### Public API
  def value(entity) do
    :gen_event.call(entity, Components.Limbs, :value)
  end

  def value(entity, new_value) do
    Entity.notify(entity, {:set_limbs, new_value})
  end

  def equip(entity, item) do
    :gen_event.call(entity, Components.Limbs, {:equip, item})
  end

  def unequip(entity, item) do
    :gen_event.call(entity, Components.Limbs, {:unequip, item})
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
    if Entity.has_component?(item, Components.WornOn) && Entity.has_component?(item, Components.Slot) do
      worn_on    = Components.WornOn.value(item)
      slot       = Components.Slot.value(item)
      open_limbs = Systems.Limbs.open_limbs(worn_on, value, slot)
      limbs_needed = Systems.Limbs.limbs_needed(worn_on, open_limbs)

      if Map.keys(limbs_needed) |> Enum.count > 0 do
        items_to_remove = Systems.Limbs.items_to_remove(limbs_needed, slot, value)
        value = Systems.Limbs.unequip_items(items_to_remove, value)
        open_limbs = Systems.Limbs.open_limbs(worn_on, value, slot)
      end

      limbs_to_use = Systems.Limbs.limbs_to_use(open_limbs, worn_on)

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

  def handle_call({:unequip, item}, value) do
    value = Systems.Limbs.unequip_items([item], value)
    {:ok, item, value}
  end

  def handle_event({:set_limbs, value}, _value) do
    {:ok, value}
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end