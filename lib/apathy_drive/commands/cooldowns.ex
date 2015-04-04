defmodule Commands.Cooldowns do
  use ApathyDrive.Command

  def keywords, do: ["cooldowns", "cd"]

  def execute(%Spirit{} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p>You need a body to do that.</p>")
  end

  def execute(%Monster{} = monster, _arguments) do
    monster
    |> Monster.send_scroll("<p><span class='white'>The following abilities are on cooldown:</span></p>")
    |> Monster.send_scroll("<p><span class='dark-magenta'>Ability Name    Remaining</span></p>")
    |> display_cooldowns

    monster
  end

  def display_cooldowns(%Monster{effects: effects} = monster) do
    effects
    |> Map.values
    |> Enum.filter(fn(effect) ->
         Map.has_key?(effect, "cooldown")
       end)
    |> Enum.each(fn(effect) ->
         Monster.send_scroll(monster, "<p><span class='dark-cyan'>#{effect["cooldown"] |> String.ljust(15)} #{effect["cooldown_remaining"]} seconds</span></p>")
       end)
  end

end
