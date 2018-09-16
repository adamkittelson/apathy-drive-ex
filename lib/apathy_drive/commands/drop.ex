defmodule ApathyDrive.Commands.Drop do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Currency, Item, ItemInstance, Match, Mobile, TimerManager, Repo}

  def keywords, do: ["drop"]

  def execute(%Room{} = room, %Character{} = character, []) do
    Mobile.send_scroll(character, "<p>Drop what?</p>")
    room
  end

  def execute(%Room{} = room, %Character{} = character, [first | rest] = arguments) do
    case Integer.parse(first) do
      {amount, ""} ->
        currency = Enum.join(rest, " ")

        Currency.matches()
        |> Match.one(:name_contains, currency)
        |> case do
          nil ->
            Mobile.send_scroll(
              character,
              "<p><span class='red'>Syntax: DROP #{amount} {Currency}</span></p>"
            )

            room

          %{name: name, currency: currency} ->
            if (current = Map.get(character, currency)) >= amount do
              room_currency = Map.get(room, currency)

              room =
                Room.update_mobile(room, character.ref, fn char ->
                  char
                  |> Ecto.Changeset.change(%{
                    currency => current - amount
                  })
                  |> Repo.update!()
                  |> Character.load_items()
                end)
                |> Ecto.Changeset.change(%{
                  currency => room_currency + amount
                })
                |> Repo.update!()

              Mobile.send_scroll(character, "<p>You dropped #{amount} #{name}s.</p>")
              room
            else
              Mobile.send_scroll(
                character,
                "<p><span class='red'>You don't have #{amount} #{name}s to drop!</span></p>"
              )

              room
            end
        end

      _ ->
        item_name = Enum.join(arguments, " ")

        character.inventory
        |> Match.one(:name_contains, item_name)
        |> case do
          nil ->
            Mobile.send_scroll(character, "<p>You don't have \"#{item_name}\" to drop!</p>")
            room

          %Item{instance_id: instance_id} = item ->
            ItemInstance
            |> Repo.get(instance_id)
            |> Ecto.Changeset.change(%{
              room_id: room.id,
              character_id: nil,
              equipped: false,
              hidden: false
            })
            |> Repo.update!()

            update_in(room.items, &[item | &1])
            |> Room.update_mobile(character.ref, fn char ->
              char =
                if {:longterm, instance_id} in TimerManager.timers(char) do
                  Mobile.send_scroll(
                    char,
                    "<p><span class='cyan'>You interrupt your work.</span></p>"
                  )

                  TimerManager.cancel(char, {:longterm, instance_id})
                else
                  char
                end

              char
              |> update_in([Access.key!(:equipment)], &List.delete(&1, item))
              |> update_in([Access.key!(:inventory)], &List.delete(&1, item))
              |> Mobile.send_scroll("<p>You drop #{Item.colored_name(item)}.</p>")
            end)
        end
    end
  end
end
