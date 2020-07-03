defmodule ApathyDrive.Commands.Pray do
  use ApathyDrive.Command
  alias ApathyDrive.{Ability, Character, Currency, Repo, Room}

  def keywords, do: ["pray"]

  def execute(%Room{} = room, %Character{} = character, _) do
    Mobile.send_scroll(
      character,
      "<p><span class='blue'>You pray for your limbs to be restored.</span></p>"
    )

    if Enum.any?(character.limbs, fn {_, limb} -> limb.health <= 0 end) do
      price_in_copper = cost(character)

      currency = Currency.set_value(price_in_copper)

      if price_in_copper > Currency.wealth(character) do
        message = "<p>You lack a sufficient offering!</p>"
        Mobile.send_scroll(character, message)
        room
      else
        char_currency = Currency.subtract(character, price_in_copper)

        character =
          character
          |> Ecto.Changeset.change(%{
            runic: char_currency.runic,
            platinum: char_currency.platinum,
            gold: char_currency.gold,
            silver: char_currency.silver,
            copper: char_currency.copper
          })
          |> Repo.update!()

        if price_in_copper > 0 do
          Mobile.send_scroll(
            character,
            "<p>You make an offering of #{Currency.to_string(currency)}.</p>"
          )
        end

        room
        |> put_in([:mobiles, character.ref], character)
        |> restore_limbs(character)
      end
    else
      Mobile.send_scroll(character, "<p>You have no missing limbs.</p>")
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

  def cost(%Character{level: level} = character) do
    charm = Mobile.attribute_at_level(character, :charm, character.level)

    multiplier =
      character
      |> Character.legal_status()
      |> multiplier()

    next_level = level + 1
    charm_mod = 1 - (trunc(charm / 5.0) - 10) / 100
    trunc(next_level * 5 * multiplier * 10 * charm_mod)
  end

  def multiplier("FIEND"), do: 15
  def multiplier("Villain"), do: 13
  def multiplier("Criminal"), do: 11
  def multiplier("Outlaw"), do: 9
  def multiplier("Seedy"), do: 7
  def multiplier("Neutral"), do: 5
  def multiplier("Good"), do: 3
  def multiplier("Saint"), do: 1
end
