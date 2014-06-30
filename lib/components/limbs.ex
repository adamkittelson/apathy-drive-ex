defmodule Components.Limbs do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Limbs, :value)
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_limbs, new_value})
  end

  def equip(entity, item) do
    GenEvent.call(entity, Components.Limbs, {:equip, item})
  end

  def unequip(entity, item) do
    GenEvent.call(entity, Components.Limbs, {:unequip, item})
  end

  def max_damage(entity, limb_name) do
    max_hp = Systems.HP.max_hp(entity)
    GenEvent.call(entity, Components.Limbs, {:max_damage, limb_name, max_hp})
  end

  def current_damage(entity, limb_name) do
    GenEvent.call(entity, Components.Limbs, {:current_damage, limb_name})
  end

  def damage_limb(entity, limb_name, amount) do
    GenEvent.notify(entity, {:damage_limb, limb_name, amount})
  end

  def heal_limb(entity, limb_name, amount) do
    GenEvent.notify(entity, {:heal_limb, limb_name, amount})
  end

  def crippled?(entity, limb_name) do
    !fatal_if_severed?(entity, limb_name) && (current_damage(entity, limb_name) || 0) >= max_damage(entity, limb_name)
  end

  def fatal_if_severed?(entity, limb_name) do
    !!value(entity)[limb_name]["fatal"]
  end

  def severed?(entity, limb_name) do
    !!value(entity)[limb_name]["severed"]
  end

  def attached(entity, limb_name) do
    value(entity)[limb_name]["attached"]
  end

  def unsevered_limbs(entity) do
    value(entity)
    |> Map.keys
    |> Enum.filter(&(!severed?(entity, &1)))
  end

  def random(entity) do
    :random.seed(:os.timestamp)
    unsevered_limbs(entity)
    |> Enum.shuffle
    |> List.first
  end

  def serialize(entity) do
    %{"Limbs" => value(entity)}
  end

  defp valid_limbs(worn_on, limbs) do
    limbs
    |> Map.keys
    |> Enum.filter(fn(limb_name) ->
         worn_on
         |> Map.keys
         |> Enum.any?(fn(worn_on_limb) ->
              Regex.match?(~r/#{worn_on_limb}/, limb_name)
            end)
       end)
  end

  defp open_limbs(worn_on, limbs, slot) do
    valid_limbs(worn_on, limbs) |> Enum.reject(fn(limb_name) ->
      limbs[limb_name]["items"] |> Enum.any?(fn(item) ->
        (Components.find_by(Components.ID, item) |> Components.Slot.value) == slot
      end)
    end) |> Enum.reduce(%{}, fn(open_limb_name, map) ->
              if Map.has_key?(map, open_limb_name) do
                Map.put(map, open_limb_name, Map.get(map, open_limb_name) + 1)
              else
                Map.put_new(map, open_limb_name, 1)
              end
            end)
  end

  defp limbs_needed(worn_on, open_limbs) do
    Enum.reduce(Map.keys(open_limbs), worn_on, fn(open_limb, needed_limbs) ->
      worn_on_key = Map.keys(needed_limbs) |> Enum.find(&(Regex.match?(~r/#{&1}/, open_limb)))
      if worn_on_key do
        needed      = Map.get(needed_limbs, worn_on_key)
        open        = Map.get(open_limbs, open_limb)
        if needed > open do
          Map.put(needed_limbs, worn_on_key, needed - open)
        else
          Map.delete(needed_limbs, worn_on_key)
        end
      else
        needed_limbs
      end
    end)
  end

  defp limbs_to_use(open_limbs, worn_on) do
    Enum.reduce(Map.keys(open_limbs), [], fn(open_limb, limbs_to_use) ->
      required_limb  = Map.keys(worn_on) |> Enum.find(&(Regex.match?(~r/#{&1}/, open_limb)))
      required_count = Map.get(worn_on, required_limb)
      filled_count = limbs_to_use |> Enum.count(&(Regex.match?(~r/#{required_limb}/, &1)))
      if required_count > filled_count do
        [open_limb | limbs_to_use]
      else
        limbs_to_use
      end
    end)
  end

  defp items_to_remove(limbs_needed, slot, limbs) do
    equipped_items = Systems.Limbs.equipped_items(limbs)
                     |> Enum.filter(&(slot == Components.Slot.value(&1)))
    initial = %{
      :limbs_needed => limbs_needed,
      :items_to_remove => [],
      :equipped_items => equipped_items
    }

    needed_limb_names = Map.get(initial, :limbs_needed)
                        |> Map.keys
                        |> Enum.map(fn(limb_name) ->
                            List.duplicate(limb_name, Map.get(initial, :limbs_needed) |> Map.get(limb_name))
                           end)
                        |> List.flatten

    map = Enum.reduce(needed_limb_names, initial, fn(needed_limb, map) ->
      if Map.get(map, :limbs_needed) |> Map.get(needed_limb) > 0 do
        item = Map.get(map, :equipped_items) |> Enum.find(fn(equipped_item) ->
          Components.WornOn.value(equipped_item) |> Map.keys |> Enum.member?(needed_limb)
        end)

        if item do
          map = Map.put(map, :items_to_remove, [item | Map.get(map, :items_to_remove)])
          map = Map.put(map, :equipped_items, List.delete(Map.get(map, :equipped_items), item))
          limbs_needed = Map.get(map, :limbs_needed)
          limbs_needed = Enum.reduce(Map.keys(limbs_needed), limbs_needed, fn(needed_limb, map) ->
            worn_on = Components.WornOn.value(item)
            if worn_on |> Map.has_key?(needed_limb) do
              Map.put(map, needed_limb, Map.get(map, needed_limb) - Map.get(worn_on, needed_limb))
            else
              map
            end
          end)
          Map.put(map, :limbs_needed, limbs_needed)
        else
          map
        end
      else
        map
      end
    end)
    Map.get(map, :items_to_remove)
  end

  defp equip_item(item, limbs_to_use, limbs) do
    Enum.reduce(limbs_to_use, limbs, fn(limb_name, limbs) ->
      limb = limbs[limb_name]
      items = limb["items"]
      items = [Components.ID.value(item) | items] |> Enum.uniq
      limb = Map.put(limb, "items", items)
      Map.put(limbs, limb_name, limb)
    end)
  end

  defp unequip_items(items_to_remove, limbs) do
    Enum.reduce(Map.keys(limbs), limbs, fn(limb_name, limbs) ->
      limb = limbs[limb_name]
      items = Enum.reduce(items_to_remove, limb["items"], fn(item_to_remove, items) ->
        List.delete(items, Components.ID.value(item_to_remove))
      end)
      limb = Map.put(limb, "items", items)
      Map.put(limbs, limb_name, limb)
    end)
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
      open_limbs = open_limbs(worn_on, value, slot)
      limbs_needed = limbs_needed(worn_on, open_limbs)

      if Map.keys(limbs_needed) |> Enum.count > 0 do
        items_to_remove = items_to_remove(limbs_needed, slot, value)
        value = unequip_items(items_to_remove, value)
        open_limbs = open_limbs(worn_on, value, slot)
      end

      limbs_to_use = limbs_to_use(open_limbs, worn_on)

      value = equip_item(item, limbs_to_use, value)

      if items_to_remove do
        {:ok, %{"removed" => items_to_remove}, value}
      else
        {:ok, %{}, value}
      end
    else
      {:ok, %{"error" => "You can't wear that."}, value}
    end
  end

  def handle_call({:max_damage, limb_name, max_hp}, value) do
    if value[limb_name]["fatal"] do
      {:ok, Float.floor(max_hp * 0.5), value}
    else
      {:ok, Float.floor(max_hp * 0.25), value}
    end
  end

  def handle_call({:unequip, item}, value) do
    value = unequip_items([item], value)
    {:ok, item, value}
  end

  def handle_call({:current_damage, limb}, value) do
    {:ok, value[limb]["damage"], value}
  end

  def handle_event({:damage_limb, limb_name, amount}, value) do
    value = update_in(value[limb_name]["damage"], fn(current_damage) ->
      if current_damage do
        current_damage + amount
      else
        amount
      end
    end)
    {:ok, value}
  end

  def handle_event({:heal_limb, limb_name, amount}, value) do
    value = update_in(value[limb_name]["damage"], fn(current_damage) ->
      if current_damage do
        Enum.max([current_damage - amount, 0])
      else
        0
      end
    end)
    {:ok, value}
  end

  def handle_event({:set_limbs, value}, _value) do
    {:ok, value}
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end