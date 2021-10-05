defmodule ApathyDrive.Commands.Get do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, CharacterItem, Currency, Item, ItemInstance, Match, Mobile, Repo}

  def keywords, do: ["get", "g"]

  def execute(%Room{} = room, %Character{} = character, []) do
    Mobile.send_scroll(character, "<p>Get what?</p>")
    room
  end

  def execute(%Room{items: items} = room, %Character{} = character, ["all"]) do
    items
    |> Enum.filter(&(&1.getable == true))
    |> Enum.map(& &1.name)
    |> get_all(room, character)
  end

  def execute(
        %Room{items: items} = room,
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
                Room.update_mobile(room, character.ref, fn _room, char ->
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
          |> Enum.filter(&(&1.getable == true))
          |> Match.one(:name_contains, item)

        currency =
          Currency.matches()
          |> Match.one(:name_contains, item)

        case actual_item || currency do
          %Item{instance_id: instance_id} = item ->
            if item.weight <=
                 Character.max_encumbrance(character) - Character.encumbrance(character) do
              room =
                if item.stackable do
                  ItemInstance
                  |> Repo.get(instance_id)
                  |> Repo.delete!()

                  room
                  |> Room.update_mobile(character.ref, fn _room, char ->
                    CharacterItem
                    |> Repo.get_by(character_id: character.id, item_id: item.id)
                    |> case do
                      %CharacterItem{count: count} = ci ->
                        ci
                        |> Ecto.Changeset.change(%{
                          count: count + 1
                        })
                        |> Repo.update!()

                      nil ->
                        %CharacterItem{character_id: character.id, item_id: item.id, count: 1}
                        |> Repo.insert!()
                    end

                    char
                    |> Mobile.send_scroll(
                      "<p>You took #{Item.colored_name(item, character: char)}.</p>"
                    )
                  end)
                else
                  ItemInstance
                  |> Repo.get(instance_id)
                  |> Ecto.Changeset.change(%{
                    room_id: nil,
                    character_id: character.id,
                    equipped: false,
                    class_id: nil,
                    hidden: false
                  })
                  |> Repo.update!()

                  room
                  |> Room.update_mobile(character.ref, fn _room, char ->
                    char
                    |> Mobile.send_scroll(
                      "<p>You took #{Item.colored_name(item, character: char)}.</p>"
                    )

                    item =
                      item
                      |> Map.put(:equipped, false)
                      |> Map.put(:hidden, false)

                    update_in(char.inventory, &[item | &1])
                  end)
                end

              room = update_in(room.items, &List.delete(&1, item))

              Room.send_scroll(
                room,
                "<p><span class='dark-yellow'>#{character.name} picks up #{Item.colored_name(item)}.</span></p>",
                [character]
              )

              room
            else
              Mobile.send_scroll(
                character,
                "<p>#{Item.colored_name(item, character: character)} is too heavy.</p>"
              )

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
