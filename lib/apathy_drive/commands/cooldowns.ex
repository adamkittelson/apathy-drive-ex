defmodule ApathyDrive.Commands.Cooldowns do
  use ApathyDrive.Command

  def keywords, do: ["cooldowns", "cd"]

  def execute(%Room{} = room, %Mobile{} = mobile, _arguments) do
    Mobile.send_scroll(mobile, "<p><span class='white'>The following abilities are on cooldown:</span></p>")
    Mobile.send_scroll(mobile, "<p><span class='dark-magenta'>Ability Name    Remaining</span></p>")

    mobile.effects
    |> Map.values
    |> Enum.filter(fn(effect) ->
         Map.has_key?(effect, "cooldown")
       end)
    |> Enum.each(fn
           %{"cooldown" => name} = effect when is_binary(name) ->
             remaining =
               mobile
               |> ApathyDrive.TimerManager.time_remaining(effect["timers"] |> List.first)
               |> div(1000)

             Mobile.send_scroll(mobile, "<p><span class='dark-cyan'>#{name |> String.ljust(15)} #{remaining} seconds</span></p>")
          _effect ->
            :noop
       end)
    room
  end

end
