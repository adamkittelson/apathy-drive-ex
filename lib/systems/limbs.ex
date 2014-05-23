defmodule Systems.Limbs do

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
    character |> Components.Limbs.value |> equipped_items
  end

  def equipped_items(limbs) do
    Map.values(limbs) |> Enum.map(&(&1["items"])) |> List.flatten |> Enum.uniq |> Enum.map(&(Components.find_by(Components.ID, &1)))
  end

end