defmodule ApathyDrive.Commands.Pray do
  use ApathyDrive.Command
  alias ApathyDrive.{Ability, Character, Repo, Room}

  def keywords, do: ["pray"]

  def execute(%Room{} = room, %Character{} = character, _) do
    if !Mobile.unconcious(character) do
      if character.evil_points < 30 do
        Mobile.send_scroll(
          character,
          "<p><span class='blue'>You pray for your limbs to be restored.</span></p>"
        )

        restore_limbs(room, character)
      else
        Mobile.send_scroll(
          character,
          "<p><span class='blue'>You pray for absolution.</span></p>"
        )

        smite(room, character)
      end
    else
      room
    end
  end

  def smite(room, character) do
    ability = %ApathyDrive.Ability{
      traits: %{"Damage" => 0.5},
      targets: "self",
      energy: 0,
      kind: "attack",
      user_message: "The gods smite you in atonement for your sins!",
      ignores_round_cooldown?: true,
      difficulty: nil
    }

    room
    |> Room.update_mobile(character.ref, &Character.alter_evil_points(&1, -20))
    |> Ability.execute(character.ref, ability, [character.ref])
  end

  def restore_limbs(room, character) do
    if Enum.any?(character.limbs, fn {_, limb} -> limb.health <= 0 end) do
      Room.update_mobile(room, character.ref, fn character ->
        character.limbs
        |> Enum.reduce(character, fn {limb_name, limb}, character ->
          if limb.health <= 0 do
            Mobile.send_scroll(character, "<p>Your #{limb_name} begins to grow back!</p>")

            effect = %{
              "StatusMessage" => "Your #{limb_name} is crippled!",
              "stack_key" => {:crippled, limb_name}
            }

            character
            |> Systems.Effect.remove_oldest_stack({:severed, limb_name})
            |> Systems.Effect.add(effect)
            |> put_in([:limbs, limb_name, :health], 0.01)
            |> Character.decrement_highest_attribute()
          else
            character
          end
        end)
        |> Ecto.Changeset.change(%{
          missing_limbs: []
        })
        |> Repo.update!()
      end)
    else
      Mobile.send_scroll(character, "<p>You have no missing limbs.</p>")
      room
    end
  end
end
