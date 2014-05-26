defmodule Systems.Limbs do

  def valid_limbs(worn_on, limbs) do
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

  def open_limbs(worn_on, limbs, slot) do
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

  def limbs_needed(worn_on, open_limbs) do
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

  def limbs_to_use(open_limbs, worn_on) do
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

  def items_to_remove(limbs_needed, slot, limbs) do
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
    character |> Components.Limbs.value |> equipped_items
  end

  def equipped_items(limbs) do
    Map.values(limbs) |> Enum.map(&(&1["items"])) |> List.flatten |> Enum.uniq |> Enum.map(&(Components.find_by(Components.ID, &1)))
  end

  def unequip_items(items_to_remove, limbs) do
    Enum.reduce(Map.keys(limbs), limbs, fn(limb_name, limbs) ->
      limb = limbs[limb_name]
      items = Enum.reduce(items_to_remove, limb["items"], fn(item_to_remove, items) ->
        List.delete(items, Components.ID.value(item_to_remove))
      end)
      limb = Map.put(limb, "items", items)
      Map.put(limbs, limb_name, limb)
    end)
  end

end
