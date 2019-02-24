defmodule ApathyDrive.Commands.Backstab do
  use ApathyDrive.Command
  alias ApathyDrive.{Ability, Match, Mobile, Party}

  def keywords, do: ["bs", "backstab"]

  def execute(%Room{} = room, %{} = character, []) do
    Mobile.send_scroll(character, "<p>Backstab what?</p>")
    room
  end

  def execute(%Room{} = room, %Character{} = character, args) do
    query =
      args
      |> Enum.join(" ")
      |> String.downcase()

    target =
      room.mobiles
      |> Map.values()
      |> Enum.reject(&(&1.ref in Party.refs(room, character)))
      |> Match.one(:name_contains, query)

    cond do
      !character.sneaking ->
        ApathyDrive.Commands.Attack.execute(room, character, args)

      is_nil(target) ->
        Mobile.send_scroll(character, "<p>You don't see #{query} here.</p>")
        room

      :else ->
        backstab(room, character, target)
    end
  end

  def backstab(room, character, target) do
    ability = Mobile.attack_ability(character)

    attacks_per_round = Float.ceil(1000 / ability.energy + 1) |> trunc

    ability =
      ability
      |> Map.put(:energy, 1000)
      |> update_in([Access.key!(:traits), "Damage"], fn damages ->
        Enum.map(damages, fn damage ->
          damage
          |> Map.put(:max, damage.max * attacks_per_round)
          |> Map.put(:min, damage.min * attacks_per_round)
        end)
      end)
      |> put_in([Access.key!(:traits), "Dodgeable"], false)
      |> Map.update!(:spectator_message, &surprise_message/1)
      |> Map.update!(:target_message, &surprise_message/1)
      |> Map.update!(:user_message, &surprise_message/1)
      |> Map.put(:attributes, %{agility: 0})

    Ability.execute(room, character.ref, ability, [target.ref])
  end

  def surprise_message(message) do
    message
    |> String.split(" ")
    |> List.insert_at(1, "surprise")
    |> Enum.join(" ")
  end
end
