defmodule ApathyDrive.Commands.Teleport do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Currency, Match, Mobile, Repo, Room}

  def keywords, do: ["teleport"]

  def execute(%Room{} = room, %Character{} = character, []) do
    character
    |> Mobile.send_scroll(
      "<p><span class='white'>You may teleport to the following locations:</span></p>"
    )
    |> Mobile.send_scroll(
      "<p><span class='dark-magenta'>Location                  Cost</span></p>"
    )

    Enum.each(character.attunements, fn attunement ->
      location = String.pad_trailing(attunement.name, 25)

      cost =
        room.coordinates
        |> cost(attunement.coordinates)
        |> Currency.to_string()

      Mobile.send_scroll(
        character,
        "<p><span class='dark-cyan'>#{location} #{cost}</span></p>"
      )
    end)

    room
  end

  def execute(%Room{} = room, %Character{} = character, location) do
    location = Enum.join(location, " ")

    character.attunements
    |> Match.all(:keyword_starts_with, location)
    |> case do
      %{} = attunement ->
        teleport(room, character, attunement)

      nil ->
        Mobile.send_scroll(
          character,
          "<p>You are not attuned to an obelisk at that location.</p>"
        )

        room

      attunements ->
        Mobile.send_scroll(
          character,
          "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"
        )

        Enum.each(attunements, fn attunement ->
          Mobile.send_scroll(
            character,
            "<p>-- #{attunement.name}</p>"
          )
        end)

        room
    end
  end

  defp teleport(room, character, attunement) do
    cost = cost(room.coordinates, attunement.coordinates)

    if Currency.wealth(cost) < Currency.wealth(character) do
      room =
        Room.update_mobile(room, character.ref, fn _room, character ->
          char_currency = Currency.subtract(character, Currency.wealth(cost))

          Mobile.send_scroll(
            character,
            "<p>The teleportation fee of #{Currency.to_string(cost)} is transported from your coinpurse.</p>"
          )

          character
          |> Ecto.Changeset.change(%{
            runic: char_currency.runic,
            platinum: char_currency.platinum,
            gold: char_currency.gold,
            silver: char_currency.silver,
            copper: char_currency.copper
          })
          |> Repo.update!()
        end)

      room_exit = %{
        "kind" => "Action",
        "destination" => attunement.room_id,
        "mover_message" =>
          "<span class='blue'>You vanish into thin air and reappear somewhere else!</span>",
        "from_message" => "<span class='blue'>{{Name}} vanishes into thin air!</span>",
        "to_message" => "<span class='blue'>{{Name}} appears out of thin air!</span>"
      }

      ApathyDrive.Commands.Move.execute(room, room.mobiles[character.ref], room_exit, 0)
    else
      Mobile.send_scroll(
        character,
        "<p>You cannot afford to teleport to #{attunement.name}.</p>"
      )

      room
    end
  end

  defp cost(current_coords, destination_coords) do
    distance =
      Enum.map(["x", "y", "z"], fn axis ->
        distance(current_coords[axis], destination_coords[axis])
      end)
      |> Enum.sum()

    Currency.set_value(distance * 100)
  end

  defp distance(current, dest) do
    max(current, dest) - min(current, dest)
  end
end
