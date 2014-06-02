defmodule Systems.Limbs do
  use Systems.Reload

  def equipped_items(character) when is_pid(character) do
    character |> Components.Limbs.value |> equipped_items
  end

  def equipped_items(limbs) do
    Map.values(limbs) |> Enum.map(&(&1["items"])) |> List.flatten |> Enum.uniq |> Enum.map(&(Components.find_by(Components.ID, &1)))
  end

  def get_limb_names(limbs, item) do
    Map.keys(limbs) |> Enum.filter(fn(limb_name) ->
      limbs[limb_name]["items"] |> Enum.member?(Components.ID.value(item))
    end)
  end

end
