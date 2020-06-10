defmodule ApathyDrive.Commands.Pray do
  use ApathyDrive.Command
  alias ApathyDrive.{Ability, Character, Repo, Room}

  def keywords, do: ["pray"]

  def execute(%Room{} = room, %Character{} = character, _) do
    if character.evil_points < 30 do
      Mobile.send_scroll(
        character,
        "<p><span class='blue'>You pray for your limbs to be restored.</span></p>"
      )

      if Enum.any?(character.limbs, fn {_, limb} -> limb.health <= 0 end) do
        room
        |> restore_limbs(character)
        |> Room.update_mobile(character.ref, fn _room, character ->
          Character.decrement_highest_attribute(character)
        end)
      else
        Mobile.send_scroll(character, "<p>You have no missing limbs.</p>")
        room
      end
    else
      Mobile.send_scroll(
        character,
        "<p><span class='blue'>You pray for absolution.</span></p>"
      )

      smite(room, character)
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
    |> Room.update_mobile(character.ref, fn _room, character ->
      Character.alter_evil_points(character, -20)
    end)
    |> Ability.execute(character.ref, ability, [character.ref])
  end

  def restore_limbs(room, character) do
    Room.update_mobile(room, character.ref, fn _room, character ->
      character.limbs
      |> Enum.reduce(character, fn {limb_name, limb}, character ->
        if limb.health <= 0 do
          Mobile.send_scroll(character, "<p>Your #{limb_name} grows back!</p>")

          character
          |> Systems.Effect.remove_oldest_stack({:severed, limb_name})
          |> put_in([:limbs, limb_name, :health], 1.0)
        else
          character
        end
      end)
      |> Ecto.Changeset.change(%{
        missing_limbs: []
      })
      |> Repo.update!()
    end)
  end
end
