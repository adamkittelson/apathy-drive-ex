defmodule ApathyDrive.Commands.Sneak do
  use ApathyDrive.Command
  alias ApathyDrive.{Aggression, Mobile}

  def keywords, do: ["sn", "sneak"]

  def execute(%Room{} = room, %Character{ref: ref} = character, _args) do
    stealth = Mobile.stealth_at_level(character, character.level)

    cond do
      stealth == 0 ->
        Mobile.send_scroll(character, "<p>You don't know how to sneak!</p>")
        room

      character.sneaking == true ->
        Room.update_mobile(room, character.ref, fn character ->
          Mobile.send_scroll(character, "<p>You are already sneaking.</p>")
        end)

      !is_nil(character.attack_target) or Aggression.enemies_present?(room, character) ->
        Mobile.send_scroll(character, "<p>You can't sneak right now!</p>")
        room

      :rand.uniform(100) < stealth ->
        Mobile.send_scroll(character, "<p>Attempting to sneak...</p>")

        room =
          Room.update_mobile(room, character.ref, fn character ->
            character
            |> Map.put(:sneaking, true)
            |> Character.add_attribute_experience(%{
              agility: 0.75,
              charm: 0.25
            })
          end)

        room =
          room.mobiles
          |> Map.values()
          |> Enum.reduce(room, fn
            %Character{ref: ^ref}, room ->
              room

            %Character{} = observer, room ->
              observer = update_in(observer.detected_characters, &MapSet.put(&1, character.ref))
              put_in(room.mobiles[observer.ref], observer)

            _, room ->
              room
          end)

        Room.update_moblist(room)

        room

      :else ->
        Mobile.send_scroll(
          character,
          "<p>Attempting to sneak...You don't think you're sneaking.</p>"
        )

        room
    end
  end
end
