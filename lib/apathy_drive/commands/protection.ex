defmodule ApathyDrive.Commands.Protection do
  use ApathyDrive.Command

  @protection_levels %{
     0 => {"grey", "none"},
     1 => {"dark-red", "very poor"},
    10 => {"red", "poor"},
    21 => {"dark-blue", "low"},
    32 => {"blue", "below average"},
    43 => {"dark-cyan", "average"},
    53 => {"cyan", "above average"},
    62 => {"dark-magenta", "good"},
    70 => {"magenta", "very good"},
    77 => {"dark-green", "extremely good"},
    83 => {"green", "superb"},
    88 => {"dark-yellow", "excellent"},
    92 => {"yellow", "awesome"},
    95 => {"dark-red", "god awesome"},
    99 => {"red", "IMPREGNABLE!"}
  }

  def keywords, do: ["protection"]

  def execute(%Mobile{} = mobile, _arguments) do
    title = "Average Protection by Damage Type"

    mobile
    |> Mobile.send_scroll("<p><span class='dark-blue'>+-------------------------------------------+</span></p>")
    |> Mobile.send_scroll("<p><span class='dark-blue'>|</span> <span class='yellow'>#{String.ljust(title, 42)}</span><span class='dark-blue'>|</span></p>")
    |> Mobile.send_scroll("<p><span class='dark-blue'>+-----------------+-------------------------+</span></p>")
    |> Mobile.send_scroll("<p><span class='dark-blue'>|</span> <span class='yellow'>Damage Type</span>     <span class='dark-blue'>|</span> <span class='yellow'>Protection Level</span>        <span class='dark-blue'>|</span></p>")
    |> Mobile.send_scroll("<p><span class='dark-blue'>+-----------------+-------------------------+</span></p>")

    ["ice", "fire", "stone", "lightning", "normal", "water", "poison"]
    |> Enum.each(fn(damage_type) ->
       {color, protection} = Mobile.protection(mobile, damage_type)
                             |> protection_level
       Mobile.send_scroll(mobile, "<p><span class='dark-blue'>|</span> <span class='yellow'>#{damage_type |> String.ljust(16)}</span><span class='dark-blue'>|</span> <span class='#{color}'>#{String.ljust(protection, 24)}</span><span class='dark-blue'>|</span></p>")
       end)
    Mobile.send_scroll(mobile, "<p><span class='dark-blue'>+-----------------+-------------------------+</span></p>")
  end

  defp protection_level(protection_amount) do
    key = @protection_levels
          |> Map.keys
          |> Enum.reverse
          |> Enum.find(fn(number) ->
               number <= (100 * protection_amount)
             end)
    @protection_levels[key || 0]
  end

end
