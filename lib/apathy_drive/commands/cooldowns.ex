defmodule ApathyDrive.Commands.Cooldowns do
  use ApathyDrive.Command

  def keywords, do: ["cooldowns", "cd"]

  def execute(%Room{} = room, %Character{} = character, _arguments) do
    Mobile.send_scroll(
      character,
      "<p><span class='white'>The following abilities are on cooldown:</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-magenta'>Ability Name    Remaining</span></p>"
    )

    character.effects
    |> Map.values()
    |> Enum.filter(fn effect ->
      Map.has_key?(effect, "cooldown")
    end)
    |> Enum.each(fn
      %{"cooldown" => name} = effect when is_binary(name) ->
        remaining =
          character
          |> ApathyDrive.TimerManager.time_remaining(effect["timers"] |> List.first())
          |> div(1000)

        Mobile.send_scroll(
          character,
          "<p><span class='dark-cyan'>#{name |> String.pad_trailing(15)} #{remaining} seconds</span></p>"
        )

      _effect ->
        :noop
    end)

    room
  end
end
