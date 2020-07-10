defmodule ApathyDrive.Commands.Rest do
  use ApathyDrive.Command
  alias ApathyDrive.{Aggression, Mobile, Regeneration}

  def keywords, do: ["rest"]

  def execute(%Room{} = room, %Character{} = character, args) do
    cond do
      taking_damage(character) or
        !is_nil(character.attack_target) or Aggression.enemies_present?(room, character) ->
        unless args[:silent],
          do: Mobile.send_scroll(character, "<p>You can't rest right now!</p>")

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

  defp taking_damage(character) do
    Regeneration.damage_effect_per_tick(character) != 0
  end
end
