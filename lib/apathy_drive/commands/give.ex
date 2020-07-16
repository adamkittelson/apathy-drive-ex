defmodule ApathyDrive.Commands.Give do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Currency, Item, ItemInstance, Match, Mobile, TimerManager, Repo}

  def keywords, do: ["give"]

  def execute(%Room{} = room, %Character{} = character, []) do
    Mobile.send_scroll(character, "<p>Give what?</p>")
    room
  end

  def execute(%Room{} = room, %Character{} = character, [first | rest] = arguments) do
    case Integer.parse(first) do
      {amount, ""} ->
        rest
        |> Enum.join(" ")
        |> String.split(" to ")
        |> case do
          [currency, target] ->
            Currency.matches()
            |> Match.one(:name_contains, currency)
            |> case do
              nil ->
                Mobile.send_scroll(
                  character,
                  "<p><span class='red'>Syntax: GIVE {amount} {currency} TO {someone}</span></p>"
                )

                room

              %{name: name, currency: currency} ->
                if (current = Map.get(character, currency)) >= amount do
                  room.mobiles
                  |> Map.values()
                  |> Enum.reject(&(&1.ref == character.ref))
                  |> Enum.filter(&(&1.__struct__ == Character))
                  |> Enum.reject(
                    &(&1.sneaking && !(&1.ref in character.detected_characters) &&
                        !(&1.ref == character.ref))
                  )
                  |> Match.one(:name_contains, target)
                  |> case do
                    %Character{} = target ->
                      target_currency = Map.get(target, currency)

                      room =
                        Room.update_mobile(room, character.ref, fn _room, char ->
                          char
                          |> Ecto.Changeset.change(%{
                            currency => current - amount
                          })
                          |> Repo.update!()
                          |> Character.load_items()
                        end)
                        |> Room.update_mobile(target.ref, fn _room, tar ->
                          tar
                          |> Ecto.Changeset.change(%{
                            currency => target_currency + amount
                          })
                          |> Repo.update!()
                          |> Character.load_items()
                        end)

                      Mobile.send_scroll(
                        character,
                        "<p>You gave #{amount} #{name}s to #{Mobile.colored_name(target)}.</p>"
                      )

                      Mobile.send_scroll(
                        target,
                        "<p>#{Mobile.colored_name(character)} gave you #{amount} #{name}s.</p>"
                      )

                      Room.send_scroll(
                        room,
                        "<p>#{Mobile.colored_name(character)} gave #{amount} #{name}s to #{
                          Mobile.colored_name(target)
                        }.</p>",
                        [character, target]
                      )

                      room

                    nil ->
                      Mobile.send_scroll(character, "<p>You don't see #{target} here.</p>")
                      room
                  end
                else
                  Mobile.send_scroll(
                    character,
                    "<p><span class='red'>You don't have #{amount} #{name}s to give!</span></p>"
                  )

                  room
                end
            end

          _ ->
            Mobile.send_scroll(
              character,
              "<p><span class='red'>Syntax: GIVE {amount} {currency} TO {someone}</span></p>"
            )

            room
        end

      _ ->
        arguments
        |> Enum.join(" ")
        |> String.split(" to ")
        |> case do
          [item_name, target] ->
            character.inventory
            |> Match.one(:name_contains, item_name)
            |> case do
              nil ->
                Mobile.send_scroll(character, "<p>You don't have \"#{item_name}\" to give!</p>")
                room

              %Item{instance_id: _instance_id} = item ->
                room.mobiles
                |> Map.values()
                |> Enum.reject(&(&1.ref == character.ref))
                |> Enum.filter(&(&1.__struct__ == Character))
                |> Enum.reject(
                  &(&1.sneaking && !(&1.ref in character.detected_characters) &&
                      !(&1.ref == character.ref))
                )
                |> Match.one(:name_contains, target)
                |> case do
                  %Character{} = target ->
                    give_item(room, character, item, target)

                  _ ->
                    Mobile.send_scroll(character, "<p>You don't see #{target} here.</p>")
                    room
                end
            end

          _ ->
            Mobile.send_scroll(
              character,
              "<p><span class='red'>Syntax: GIVE {item} TO {someone}</span></p>"
            )

            room
        end
    end
  end

  def give_item(room, character, %Item{instance_id: instance_id} = item, target) do
    ItemInstance
    |> Repo.get(instance_id)
    |> Ecto.Changeset.change(%{
      character_id: target.id
    })
    |> Repo.update!()

    room =
      room
      |> Room.update_mobile(character.ref, fn _room, char ->
        char =
          if {:longterm, instance_id} in TimerManager.timers(char) do
            Character.send_chat(
              char,
              "<p><span class='cyan'>You interrupt your work.</span></p>"
            )

            TimerManager.cancel(char, {:longterm, instance_id})
          else
            char
          end

        update_in(char.inventory, &List.delete(&1, item))
      end)
      |> Room.update_mobile(target.ref, fn _room, tar ->
        update_in(tar.inventory, &[item | &1])
      end)

    Mobile.send_scroll(
      character,
      "<p>You gave #{Item.colored_name(item, character: character)} to #{
        Mobile.colored_name(target)
      }.</p>"
    )

    Mobile.send_scroll(
      target,
      "<p>#{Mobile.colored_name(character)} gave #{Item.colored_name(item, character: target)} to you.</p>"
    )

    char_ref = character.ref
    tar_ref = target.ref

    Enum.each(room.mobiles, fn
      {ref, %Character{} = mobile} when ref != char_ref and ref != tar_ref ->
        Mobile.send_scroll(
          mobile,
          "<p>#{Mobile.colored_name(character)} dropped #{
            Item.colored_name(item, character: mobile)
          }.</p>"
        )

      _ ->
        :noop
    end)

    room
  end
end
