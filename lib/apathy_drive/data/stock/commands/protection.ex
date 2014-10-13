defmodule Commands.Protection do
  use Systems.Command

  @protection_levels %{
     0 => {"grey", "none"},
     7 => {"dark-red", "very poor"},
    20 => {"red", "poor"},
    32 => {"dark-blue", "low"},
    44 => {"blue", "below average"},
    54 => {"dark-cyan", "average"},
    63 => {"cyan", "above average"},
    71 => {"dark-magenta", "good"},
    78 => {"magenta", "very good"},
    84 => {"dark-green", "extremely good"},
    89 => {"green", "superb"},
    93 => {"dark-yellow", "excellent"},
    96 => {"yellow", "awesome"},
    98 => {"dark-red", "god awesome"},
    99 => {"red", "IMPREGNABLE!"}
  }

  def keywords, do: ["protection"]

  def execute(spirit, nil, arguments) do
    send_message(spirit, "scroll", "<p>Spirits cannot be harmed.</p>")
  end

  def execute(_spirit, monster, arguments) do
    protection(monster, Enum.join(arguments, " "))
  end

  def protection(entity, "limb") do
    show_protection(entity, "limb")
  end

  def protection(entity, "") do
    show_protection(entity)
  end

  def protection(entity, target) do
    limb = entity
           |> Components.Limbs.unsevered_limbs
           |> Enum.find(&(&1 == target))
    if limb do
      show_protection(entity, limb)
    else
      item_protection(entity, target)
    end
  end

  defp item_protection(entity, target) do
    shop_items = if Parent.of(entity) |> Entity.has_component?(Components.Shop) do
       entity
       |> Parent.of
       |> Components.Shop.value
       |> Enum.map(&ItemTemplates.find_by_id(&1["item"]))
    else
      []
    end
    equipped_items = Systems.Limbs.equipped_items(entity)
    inventory_items = Components.Items.get_items(entity)
    items = [shop_items, equipped_items, inventory_items]
            |> List.flatten
            |> Enum.uniq

    item = Systems.Match.one(items, :name_contains, target)

    if item do
      show_protection(entity, item)
    else
      send_message(entity, "scroll", "<p>You don't have a #{target}!</p>")
    end
  end

  defp show_protection(entity, "limb") do
    title = "Average Protection by Limb"

    send_message(entity, "scroll", "<p><span class='dark-blue'>+-------------------------------------------+</span></p>")
    send_message(entity, "scroll", "<p><span class='dark-blue'>|</span> <span class='yellow'>#{String.ljust(title, 42)}</span><span class='dark-blue'>|</span></p>")
    send_message(entity, "scroll", "<p><span class='dark-blue'>+-----------------+-------------------------+</span></p>")
    send_message(entity, "scroll", "<p><span class='dark-blue'>|</span> <span class='yellow'>Limb</span>            <span class='dark-blue'>|</span> <span class='yellow'>Protection Level</span>        <span class='dark-blue'>|</span></p>")
    send_message(entity, "scroll", "<p><span class='dark-blue'>+-----------------+-------------------------+</span></p>")

    entity
    |> Components.Limbs.unsevered_limbs
    |> Enum.each(fn(limb) ->
         {color, protection} = limb_protection_amount(entity, limb)
                               |> protection_level
         send_message(entity, "scroll", "<p><span class='dark-blue'>|</span> <span class='yellow'>#{String.ljust(limb, 16)}</span><span class='dark-blue'>|</span> <span class='#{color}'>#{String.ljust(protection, 24)}</span><span class='dark-blue'>|</span></p>")
       end)

    send_message(entity, "scroll", "<p><span class='dark-blue'>+-----------------+-------------------------+</span></p>")
  end

  defp show_protection(entity, item) when is_pid item do

    ac = cond do
      Entity.has_component?(item, Components.AC) ->
        Components.AC.value(item)
      Entity.has_component?(item, Components.Module) ->
        Components.Module.value(item).properties[:ac] || 0
      true ->
        0
    end

    title = "Protection for #{Components.Name.value(item)} by Damage Type"

    send_message(entity, "scroll", "<p><span class='dark-blue'>+-------------------------------------------+</span></p>")
    send_message(entity, "scroll", "<p><span class='dark-blue'>|</span> <span class='yellow'>#{String.ljust(title, 42)}</span><span class='dark-blue'>|</span></p>")
    send_message(entity, "scroll", "<p><span class='dark-blue'>+-----------------+-------------------------+</span></p>")
    send_message(entity, "scroll", "<p><span class='dark-blue'>|</span> <span class='yellow'>Damage Type</span>     <span class='dark-blue'>|</span> <span class='yellow'>Protection Level</span>        <span class='dark-blue'>|</span></p>")
    send_message(entity, "scroll", "<p><span class='dark-blue'>+-----------------+-------------------------+</span></p>")
    CritTables.damage_types
    |> Map.keys
    |> Enum.each(fn(damage_type) ->
       {color, protection} = protection_amount(ac)
                             |> protection_level
       send_message(entity, "scroll", "<p><span class='dark-blue'>|</span> <span class='yellow'>#{damage_type |> Atom.to_string |> String.ljust(16)}</span><span class='dark-blue'>|</span> <span class='#{color}'>#{String.ljust(protection, 24)}</span><span class='dark-blue'>|</span></p>")
       end)
    send_message(entity, "scroll", "<p><span class='dark-blue'>+-----------------+-------------------------+</span></p>")
  end

  defp show_protection(entity, limb) do
    titleized_limb = limb
                     |> String.split(" ")
                     |> Enum.map(&String.capitalize(&1))
                     |> Enum.join(" ")

    title = "Protection for #{titleized_limb} by Damage Type"

    send_message(entity, "scroll", "<p><span class='dark-blue'>+-------------------------------------------+</span></p>")
    send_message(entity, "scroll", "<p><span class='dark-blue'>|</span> <span class='yellow'>#{String.ljust(title, 42)}</span><span class='dark-blue'>|</span></p>")
    send_message(entity, "scroll", "<p><span class='dark-blue'>+-----------------+-------------------------+</span></p>")
    send_message(entity, "scroll", "<p><span class='dark-blue'>|</span> <span class='yellow'>Damage Type</span>     <span class='dark-blue'>|</span> <span class='yellow'>Protection Level</span>        <span class='dark-blue'>|</span></p>")
    send_message(entity, "scroll", "<p><span class='dark-blue'>+-----------------+-------------------------+</span></p>")
    CritTables.damage_types
    |> Map.keys
    |> Enum.each(fn(damage_type) ->
       {color, protection} = protection_amount(entity, limb, damage_type)
                             |> protection_level
       send_message(entity, "scroll", "<p><span class='dark-blue'>|</span> <span class='yellow'>#{damage_type |> String.ljust(16)}</span><span class='dark-blue'>|</span> <span class='#{color}'>#{String.ljust(protection, 24)}</span><span class='dark-blue'>|</span></p>")
       end)
    send_message(entity, "scroll", "<p><span class='dark-blue'>+-----------------+-------------------------+</span></p>")
  end

  defp show_protection(entity) do
    title = "Average Protection by Damage Type"

    send_message(entity, "scroll", "<p><span class='dark-blue'>+-------------------------------------------+</span></p>")
    send_message(entity, "scroll", "<p><span class='dark-blue'>|</span> <span class='yellow'>#{String.ljust(title, 42)}</span><span class='dark-blue'>|</span></p>")
    send_message(entity, "scroll", "<p><span class='dark-blue'>+-----------------+-------------------------+</span></p>")
    send_message(entity, "scroll", "<p><span class='dark-blue'>|</span> <span class='yellow'>Damage Type</span>     <span class='dark-blue'>|</span> <span class='yellow'>Protection Level</span>        <span class='dark-blue'>|</span></p>")
    send_message(entity, "scroll", "<p><span class='dark-blue'>+-----------------+-------------------------+</span></p>")
    CritTables.damage_types
    |> Map.keys
    |> Enum.each(fn(damage_type) ->
       {color, protection} = protection_amount(entity, damage_type)
                             |> protection_level
       send_message(entity, "scroll", "<p><span class='dark-blue'>|</span> <span class='yellow'>#{damage_type |> String.ljust(16)}</span><span class='dark-blue'>|</span> <span class='#{color}'>#{String.ljust(protection, 24)}</span><span class='dark-blue'>|</span></p>")
       end)
    send_message(entity, "scroll", "<p><span class='dark-blue'>+-----------------+-------------------------+</span></p>")
  end

  defp protection_amount(entity, limb, damage_type) do
    Systems.Damage.damage_reduction(entity, limb, damage_type)
  end

  defp protection_amount(entity, damage_type) do
    total = entity
            |> Components.Limbs.unsevered_limbs
            |> Enum.map(&protection_amount(entity, &1, damage_type))
            |> Enum.sum

     total / (CritTables.damage_types |> Map.size)
  end

  defp protection_amount(ac) do
    Systems.Damage.resistance(ac) |> Systems.Damage.resistance_reduction
  end

  defp limb_protection_amount(entity, limb) do
    total = CritTables.damage_types
            |> Map.keys
            |> Enum.map(&protection_amount(entity, limb, &1))
            |> Enum.sum

     total / Map.size(CritTables.damage_types)
  end

  defp protection_level(protection_amount) do
    key = @protection_levels
          |> Map.keys
          |> Enum.reverse
          |> Enum.find(fn(number) ->
               number <= (100 * protection_amount)
             end)
    @protection_levels[key]
  end

end
