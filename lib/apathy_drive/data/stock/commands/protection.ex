defmodule Commands.Protection do
  use Systems.Command

  @damage_types %{
    aether: "magical",
    cold: "magical",
    crushing: "physical",
    cutting: "physical",
    disruption: "magical",
    electricity: "magical",
    fire: "magical",
    holy: "magical",
    impact: "physical",
    impaling: "physical",
    infernal: "magical",
    plasma: "magical",
    strike: "physical",
    vacuum: "magical",
  }

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

  def execute(entity, arguments) do

    if Enum.any? arguments do
      requested_limb = Enum.join(arguments, " ")
      limb = entity
             |> Components.Limbs.unsevered_limbs
             |> Enum.find(&(&1 == requested_limb))
      if limb do
        show_protection(entity, limb)
      else
        send_message(entity, "scroll", "<p>You don't have a #{requested_limb}!</p>")
      end
    else
      show_protection(entity)
    end
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
    @damage_types
    |> Map.keys
    |> Enum.each(fn(damage_type) ->
       {color, protection} = protection_amount(entity, limb, damage_type)
                             |> protection_level
       send_message(entity, "scroll", "<p><span class='dark-blue'>|</span> <span class='yellow'>#{damage_type |> Atom.to_string |> String.ljust(16)}</span><span class='dark-blue'>|</span> <span class='#{color}'>#{String.ljust(protection, 24)}</span><span class='dark-blue'>|</span></p>")
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
    @damage_types
    |> Map.keys
    |> Enum.each(fn(damage_type) ->
       {color, protection} = protection_amount(entity, damage_type)
                             |> protection_level
       send_message(entity, "scroll", "<p><span class='dark-blue'>|</span> <span class='yellow'>#{damage_type |> Atom.to_string |> String.ljust(16)}</span><span class='dark-blue'>|</span> <span class='#{color}'>#{String.ljust(protection, 24)}</span><span class='dark-blue'>|</span></p>")
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

     total / Map.size(@damage_types)
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
