defmodule ApathyDrive.Commands.Rest do
  use ApathyDrive.Command
  alias ApathyDrive.{Aggression, Mobile}

  def keywords, do: ["rest"]

  def execute(%Room{} = room, %Character{} = character, _args) do
    cond do
      !is_nil(character.attack_target) or Aggression.enemies_present?(room, character) ->
        Mobile.send_scroll(character, "<p>You can't rest right now!</p>")
        room

      :else ->
        Mobile.send_scroll(character, "<p>You are now resting.</p>")

        Room.update_mobile(room, character.ref, fn _room, character ->
          character
          |> Map.put(:resting, true)
          |> Map.put(:sneaking, false)
        end)
    end
  end
end
