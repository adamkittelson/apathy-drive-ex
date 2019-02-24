defmodule ApathyDrive.Commands.Sneak do
  use ApathyDrive.Command
  alias ApathyDrive.{Aggression, Mobile, Stealth}

  def keywords, do: ["sn", "sneak"]

  def execute(%Room{} = room, %Character{} = character, _args) do
    stealth = Mobile.stealth_at_level(character, character.level)

    cond do
      stealth == 0 ->
        Mobile.send_scroll(character, "<p>You don't know how to sneak!</p>")
        room

      character.sneaking == true ->
        Room.update_mobile(room, character.ref, fn character ->
          Mobile.send_scroll(character, "<p>You are no longer sneaking.</p>")
          Stealth.reveal(character)
        end)

      is_nil(character.attack_target) and !enemies_present?(room, character) ->
        Mobile.send_scroll(character, "<p>You can't sneak right now!</p>")
        room

      :rand.uniform(100) < stealth ->
        Mobile.send_scroll(character, "<p>Attempting to sneak...</p>")

        Room.update_mobile(room, character.ref, fn character ->
          Map.put(character, :sneaking, true)
        end)

      :else ->
        Mobile.send_scroll(
          character,
          "<p>Attempting to sneak...You don't think you're sneaking.</p>"
        )

        room
    end
  end

  defp enemies_present?(room, character) do
    room.mobiles
    |> Map.values()
    |> Enum.any?(&Aggression.enemy?(&1, character))
  end
end
