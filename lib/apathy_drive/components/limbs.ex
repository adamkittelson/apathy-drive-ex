defmodule Components.Limbs do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    get_items(entity)
    GenEvent.call(entity, Components.Limbs, :value)
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_limbs, new_value})
  end

  def get_items(entity) do
    GenEvent.call(entity, Components.Limbs, :get_items)
  end

  def get_item(item) when is_pid(item), do: item
  def get_item(item) when is_number(item) do
    Items.find_by_id(item)
  end

  def equip(entity, item) do
    GenEvent.call(entity, Components.Limbs, {:equip, item})
  end

  def items(entity, limb) do
    value(entity)[limb]["items"]
    |> Enum.map(&get_item/1)
  end

  def unequip(entity, item) do
    GenEvent.call(entity, Components.Limbs, {:unequip, item})
  end

  def max_damage(entity, limb_name) do
    max_hp = Systems.HP.max(entity)
    GenEvent.call(entity, Components.Limbs, {:max_damage, limb_name, max_hp})
  end

  def current_damage(entity, limb_name) do
    GenEvent.call(entity, Components.Limbs, {:current_damage, limb_name})
  end

  def damage_limb(entity, limb_name, amount) do
    GenEvent.notify(entity, {:damage_limb, limb_name, amount})
    HPRegen.add(entity)
  end

  def heal_limb(entity, limb_name, amount) do
    GenEvent.notify(entity, {:heal_limb, limb_name, amount})
  end

  def sever_limb(entity, limb_name) do
    GenEvent.notify(entity, {:sever_limb, limb_name})
  end

  def crippled?(entity, limb_name) do
    !fatal_if_severed?(entity, limb_name) && (current_damage(entity, limb_name) || 0) >= max_damage(entity, limb_name)
  end

  def injured?(entity, limb_name) do
    !(current_damage(entity, limb_name) == 0)
  end

  def injured?(entity) do
    entity
    |> unsevered_limbs
    |> Enum.any?(&(injured?(entity, &1)))
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

  def unsevered_limbs(entity, "non_fatal") do
    unsevered_limbs(entity)
    |> Enum.filter(fn(limb_name) ->
         !fatal_if_severed?(entity, limb_name)
       end)
  end

  def unsevered_limbs(entity, limb_type) do
    unsevered_limbs(entity)
    |> Enum.filter(fn(limb_name) ->
         value(entity)[limb_name]["type"] == limb_type
       end)
  end

  def random_unsevered_limb(entity) do
    :random.seed(:os.timestamp)
    unsevered_limbs(entity)
    |> Enum.shuffle
    |> List.first
  end

  def uncrippled_limbs(entity) do
    unsevered_limbs(entity)
    |> Enum.filter(&(!crippled?(entity, &1)))
  end

  def uncrippled_limbs(entity, "non_fatal") do
    unsevered_limbs(entity, "non_fatal")
    |> Enum.filter(&(!crippled?(entity, &1)))
  end

  def uncrippled_limbs(entity, limb_type) do
    unsevered_limbs(entity, limb_type)
    |> Enum.filter(&(!crippled?(entity, &1)))
  end

  def limbs_with_item_ids(entity) do
    limbs = value(entity)

    limbs
    |> Map.keys
    |> Enum.reduce(limbs, fn(limb_name, limbs) ->
         update_in(limbs, [limb_name, "items"], &item_ids/1)
       end)
  end

  def item_ids(items) do
    items
    |> Enum.map(fn(item) ->
         cond do
           is_pid(item) ->
             cond do
               Entity.has_component?(item, Components.ID) ->
                 Components.ID.value(item)
               true ->
                 Entities.save!(item)
                 Components.ID.value(item)
             end
           is_integer(item) ->
             if Items.find_by_id(item) do
               item
             end
         end
       end)
    |> Enum.filter(&(&1 != nil))
  end

  def serialize(entity) do
    %{"Limbs" => limbs_with_item_ids(entity)}
  end

  defp valid_limbs(worn_on, limbs) do
    limbs
    |> Map.keys
    |> Enum.filter(fn(limb_name) ->
         worn_on
         |> Map.keys
         |> Enum.any?(fn(worn_on_limb) ->
              limbs[limb_name]["type"] == worn_on_limb
            end)
       end)
  end

  defp open_limbs(worn_on, limbs, slot) do
    valid_limbs(worn_on, limbs) |> Enum.reject(fn(limb_name) ->
      limbs[limb_name]["items"] |> Enum.any?(fn(item) ->
        (get_item(item) |> Components.Slot.value) == slot
      end)
    end)
  end

  defp limbs_needed(worn_on, open_limbs, limbs) do
    Enum.reduce(open_limbs, worn_on, fn(open_limb, needed_limbs) ->
      worn_on_key = Map.keys(needed_limbs) |> Enum.find(&(&1 == limbs[open_limb]["type"]))
      if worn_on_key do
        needed      = Map.get(needed_limbs, worn_on_key)
        if needed > 1 do
          Map.put(needed_limbs, worn_on_key, needed - 1)
        else
          Map.delete(needed_limbs, worn_on_key)
        end
      else
        needed_limbs
      end
    end)
  end

  defp limbs_to_use(open_limbs, worn_on, limbs) do
    Enum.reduce(open_limbs, [], fn(open_limb, limbs_to_use) ->
      required_limb  = Map.keys(worn_on) |> Enum.find(&(limbs[open_limb]["type"] == &1))
      required_count = Map.get(worn_on, required_limb)
      filled_count = limbs_to_use |> Enum.count(&(limbs[&1]["type"] == required_limb))
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

      items = [item | items] |> Enum.uniq
      limb = Map.put(limb, "items", items)
      Map.put(limbs, limb_name, limb)
    end)
  end

  defp unequip_items(items_to_remove, limbs) do
    Enum.reduce(Map.keys(limbs), limbs, fn(limb_name, limbs) ->
      limb = limbs[limb_name]
      items = Enum.reduce(items_to_remove, limb["items"], fn(item_to_remove, items) ->
        List.delete(items, get_item(item_to_remove))
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

  def handle_call(:get_items, limbs) do
    limbs = limbs
            |> Map.keys
            |> Enum.reduce(limbs, fn(limb_name, updated_limbs) ->
                 update_in(updated_limbs, [limb_name, "items"], fn(items) ->
                   items
                   |> Enum.map(&get_item/1)
                   |> Enum.filter(&(&1 != nil))
                 end)
               end)

    items = limbs
            |> Map.values
            |> Enum.map(&(&1["items"]))
            |> List.flatten
            |> Enum.uniq

    {:ok, items, limbs}
  end

  def handle_call({:equip, item}, value) do
    if Entity.has_component?(item, Components.WornOn) && Entity.has_component?(item, Components.Slot) do
      worn_on    = Components.WornOn.value(item)
      slot       = Components.Slot.value(item)
      open_limbs = open_limbs(worn_on, value, slot)
      limbs_needed = limbs_needed(worn_on, open_limbs, value)

      if Map.keys(limbs_needed) |> Enum.count > 0 do
        items_to_remove = items_to_remove(limbs_needed, slot, value)
        value = unequip_items(items_to_remove, value)
        open_limbs = open_limbs(worn_on, value, slot)
      end

      limbs_to_use = limbs_to_use(open_limbs, worn_on, value)

      if limbs_to_use == [] do
        {:ok, %{"error" => "You don't have the right limbs to equip that."}, value}
      else
        value = equip_item(item, limbs_to_use, value)

        if items_to_remove do
          {:ok, %{"removed" => items_to_remove}, value}
        else
          {:ok, %{}, value}
        end
      end
    else
      {:ok, %{"error" => "You can't wear that."}, value}
    end
  end

  def handle_call({:max_damage, limb_name, max_hp}, value) do
    if value[limb_name]["fatal"] do
      {:ok, trunc(max_hp * 0.5), value}
    else
      {:ok, trunc(max_hp * 0.25), value}
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

  def handle_event({:sever_limb, limb}, value) do
    {:ok, put_in(value[limb]["severed"], true)}
  end

  def handle_event({:set_limbs, value}, _value) do
    {:ok, value}
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end