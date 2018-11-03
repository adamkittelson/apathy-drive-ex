defmodule ApathyDrive.Commands.Get do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Currency, Item, ItemInstance, Match, Mobile, Repo}

  def keywords, do: ["get"]

  def execute(%Room{} = room, %Character{} = character, []) do
    Mobile.send_scroll(character, "<p>Get what?</p>")
    room
  end

  def execute(%Room{items: items} = room, %Character{} = character, ["all"]) do
    items
    |> Enum.map(& &1.name)
    |> get_all(room, character)
  end

  def execute(
        %Room{items: items, item_descriptions: item_descriptions} = room,
        %Character{} = character,
        [first | rest] = args
      ) do
    case Integer.parse(first) do
      {amount, ""} ->
        currency = Enum.join(rest, " ")

        Currency.matches()
        |> Match.one(:name_contains, currency)
        |> case do
          nil ->
            Mobile.send_scroll(
              character,
              "<p><span class='red'>Syntax: GET #{amount} {Currency}</span></p>"
            )

            room

          %{name: name, currency: currency} ->
            if (room_currency = Map.get(room, currency)) >= amount do
              current = Map.get(character, currency)

              max = (Character.max_encumbrance(character) - Character.encumbrance(character)) * 3

              amount = min(amount, max)

              room =
                Room.update_mobile(room, character.ref, fn char ->
                  char
                  |> Ecto.Changeset.change(%{
                    currency => current + amount
                  })
                  |> Repo.update!()
                  |> Character.load_items()
                end)
                |> Ecto.Changeset.change(%{
                  currency => room_currency - amount
                })
                |> Repo.update!()

              Mobile.send_scroll(character, "<p>You picked up #{amount} #{name}s.</p>")

              Room.send_scroll(
                room,
                "<p><span class='dark-yellow'>#{character.name} picked up some #{name}s.</span></p>",
                [character]
              )

              room
            else
              Mobile.send_scroll(
                character,
                "<p>You don't see #{amount} #{name}s.</p>"
              )

              room
            end
        end

      _ ->
        item = Enum.join(args, " ")

        actual_item =
          items
          |> Enum.filter(
            &(&1.dropped_for_character_id == character.id or is_nil(&1.dropped_for_character_id))
          )
          |> Match.one(:name_contains, item)

        visible_item =
          item_descriptions["visible"]
          |> Map.keys()
          |> Enum.map(&%{name: &1, keywords: String.split(&1)})
          |> Match.one(:keyword_starts_with, item)

        hidden_item =
          item_descriptions["hidden"]
          |> Map.keys()
          |> Enum.map(&%{name: &1, keywords: String.split(&1)})
          |> Match.one(:keyword_starts_with, item)

        currency =
          Currency.matches()
          |> Match.one(:name_contains, item)

        case actual_item || currency || visible_item || hidden_item do
          %Item{instance_id: instance_id} = item ->
            if item.weight <=
                 Character.max_encumbrance(character) - Character.encumbrance(character) do
              ItemInstance
              |> Repo.get(instance_id)
              |> Ecto.Changeset.change(%{
                room_id: nil,
                character_id: character.id,
                dropped_for_character_id: nil,
                equipped: false,
                hidden: false
              })
              |> Repo.update!()

              room =
                room
                |> Room.load_items()
                |> Room.update_mobile(character.ref, fn char ->
                  char
                  |> Character.load_items()
                  |> Mobile.send_scroll("<p>You took #{Item.colored_name(item)}.</p>")
                end)

              Room.send_scroll(
                room,
                "<p><span class='dark-yellow'>#{character.name} picks up #{
                  Item.colored_name(item)
                }.</span></p>",
                [character]
              )

              room
            else
              Mobile.send_scroll(character, "<p>#{Item.colored_name(item)} is too heavy.</p>")
              room
            end

          %{name: name, currency: currency} ->
            amount = Map.get(room, currency)
            execute(room, character, ["#{amount}", "#{name}"])

          %{name: psuedo_item} ->
            Mobile.send_scroll(
              character,
              "<p>#{psuedo_item |> capitalize_first} cannot be picked up.</p>"
            )

            room

          nil ->
            Mobile.send_scroll(character, "<p>You don't see \"#{item}\" here.</p>")
            room
        end
    end
  end

  defp get_all([], room, character) do
    Mobile.send_scroll(character, "<p>There is nothing here to get.</p>")
    room
  end

  defp get_all(item_names, room, character) do
    item_names
    |> Enum.reduce(room, fn item_name, updated_room ->
      character = updated_room.mobiles[character.ref]
      execute(updated_room, character, [item_name])
    end)
  end
end
