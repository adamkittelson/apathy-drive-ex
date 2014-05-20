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

  def valid_limbs(worn_on, limbs) do
    limbs
    |> Map.keys
    |> Enum.filter(fn(limb_name) ->
      Enum.any?(worn_on, fn(worn_on_limb) ->
        Regex.match?(~r/#{worn_on_limb}/, limb_name)
      end)
    end)
  end

  def open_limbs(worn_on, limbs, slot) do
    valid_limbs(worn_on, limbs) |> Enum.reject(fn(limb_name) ->
      limbs[limb_name]["items"] |> Enum.any?(fn(item) ->
        (Components.find_by(Components.ID, item) |> Components.Slot.value) == slot
      end)
    end)
  end

  def full_limbs(worn_on, limbs, slot) do
    valid_limbs(worn_on, limbs) |> Enum.filter(fn(limb_name) ->
      limbs[limb_name]["items"] |> Enum.any?(fn(item) ->
        (Components.find_by(Components.ID, item) |> Components.Slot.value) == slot
      end)
    end)
  end

  def equip_item(item, limbs_to_use, limbs) do
    Enum.reduce(limbs_to_use, limbs, fn(limb_name, limbs) ->
      limb = limbs[limb_name]
      items = limb["items"]
      items = [Components.ID.value(item) | items] |> Enum.uniq
      limb = Map.put(limb, "items", items)
      Map.put(limbs, limb_name, limb)
    end)
  end

  def get_limb_names(limbs, item) do
    Map.keys(limbs) |> Enum.filter(fn(limb_name) ->
      limbs[limb_name]["items"] |> Enum.member?(Components.ID.value(item))
    end)
  end

  def equipped_items(character) when is_pid(character) do
    character |> value |> equipped_items
  end

  def equipped_items(limbs) do
    Map.values(limbs) |> Enum.map(&(&1["items"])) |> List.flatten |> Enum.uniq |> Enum.map(&(Components.find_by(Components.ID, &1)))
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
      open_limbs = Components.Limbs.open_limbs(worn_on, value, slot)
      if Enum.count(open_limbs) < Enum.count(worn_on) do
        item_to_remove = Enum.find(equipped_items(value), fn(equipped_item) ->
          slot == Components.Slot.value(equipped_item)
        end)
        value = Enum.reduce(Map.keys(value), value, fn(limb_name, value) ->
          limb = value[limb_name]
          items = limb["items"]
          items = List.delete(items, Components.ID.value(item_to_remove))
          limb = Map.put(limb, "items", items)
          Map.put(value, limb_name, limb)
        end)
        open_limbs = Components.Limbs.open_limbs(worn_on, value, slot)
      end
      limbs_needed = Enum.count(worn_on)
      limbs_to_use = open_limbs |> Enum.take(limbs_needed)
      value = equip_item(item, limbs_to_use, value)

      if item_to_remove do
        {:ok, %{"removed" => item_to_remove}, value}
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