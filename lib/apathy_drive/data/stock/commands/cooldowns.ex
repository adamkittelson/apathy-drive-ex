defmodule Commands.Cooldowns do
  use Systems.Command

  def keywords, do: ["cooldowns", "cd"]

  def execute(spirit, nil, _arguments) do
    send_message(spirit, "scroll", "<p><span class='white'>The following abilities are on cooldown:</span></p>")
    send_message(spirit, "scroll", "<p><span class='dark-magenta'>Ability Name    Remaining</span></p>")
  end

  def execute(spirit, monster, _arguments) do
    send_message(spirit, "scroll", "<p><span class='white'>The following abilities are on cooldown:</span></p>")
    send_message(spirit, "scroll", "<p><span class='dark-magenta'>Ability Name    Remaining</span></p>")
    display_cooldowns(monster)
  end

  def display_cooldowns(entity) do
    entity
    |> Components.Effects.value
    |> Map.values
    |> Enum.filter(fn(effect) ->
         Map.has_key?(effect, :cooldown)
       end)
    |> Enum.each(fn(effect) ->
         send_message(entity, "scroll", "<p><span class='dark-cyan'>#{effect[:cooldown] |> String.ljust(15)} #{effect[:cooldown_remaining]} seconds</span></p>")
       end)
  end

end
