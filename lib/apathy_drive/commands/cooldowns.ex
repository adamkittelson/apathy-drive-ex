defmodule ApathyDrive.Commands.Cooldowns do
  use ApathyDrive.Command

  def keywords, do: ["cooldowns", "cd"]

  def execute(%Room{} = room, %Monster{} = monster, _arguments) do
    Monster.send_scroll(monster, "<p><span class='white'>The following abilities are on cooldown:</span></p>")
    Monster.send_scroll(monster, "<p><span class='dark-magenta'>Ability Name    Remaining</span></p>")

    monster.effects
    |> Map.values
    |> Enum.filter(fn(effect) ->
         Map.has_key?(effect, "cooldown")
       end)
    |> Enum.each(fn
           %{"cooldown" => name} = effect when is_binary(name) ->
             remaining =
               monster
               |> ApathyDrive.TimerManager.time_remaining(effect["timers"] |> List.first)
               |> div(1000)

             Monster.send_scroll(monster, "<p><span class='dark-cyan'>#{name |> String.ljust(15)} #{remaining} seconds</span></p>")
          _effect ->
            :noop
       end)
    room
  end

end
