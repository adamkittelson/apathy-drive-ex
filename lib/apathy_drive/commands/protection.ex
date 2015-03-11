defmodule Commands.Protection do
  use ApathyDrive.Command

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

  def execute(%Spirit{} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p>You need a body to do that.</p>")
  end

  def execute(%Monster{} = monster, _arguments) do
    title = "Average Protection by Damage Type"

    monster
    |> Monster.send_scroll("<p><span class='dark-blue'>+-------------------------------------------+</span></p>")
    |> Monster.send_scroll("<p><span class='dark-blue'>|</span> <span class='yellow'>#{String.ljust(title, 42)}</span><span class='dark-blue'>|</span></p>")
    |> Monster.send_scroll("<p><span class='dark-blue'>+-----------------+-------------------------+</span></p>")
    |> Monster.send_scroll("<p><span class='dark-blue'>|</span> <span class='yellow'>Damage Type</span>     <span class='dark-blue'>|</span> <span class='yellow'>Protection Level</span>        <span class='dark-blue'>|</span></p>")
    |> Monster.send_scroll("<p><span class='dark-blue'>+-----------------+-------------------------+</span></p>")

    ["ice", "fire", "stone", "lightning", "normal", "water", "poison"]
    |> Enum.each(fn(damage_type) ->
       {color, protection} = Monster.protection(monster, damage_type)
                             |> protection_level
       Monster.send_scroll(monster, "<p><span class='dark-blue'>|</span> <span class='yellow'>#{damage_type |> String.ljust(16)}</span><span class='dark-blue'>|</span> <span class='#{color}'>#{String.ljust(protection, 24)}</span><span class='dark-blue'>|</span></p>")
       end)
    Monster.send_scroll(monster, "<p><span class='dark-blue'>+-----------------+-------------------------+</span></p>")
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
