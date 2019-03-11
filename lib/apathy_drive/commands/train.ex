defmodule ApathyDrive.Commands.Train do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Currency, Directory, Repo, Trainer}

  def keywords, do: ["train"]

  def execute(%Room{} = room, %Character{} = character, _args) do
    cond do
      !Trainer.trainer?(room) ->
        message = "<p>You must be in an appropriate training room to train!</p>"
        Mobile.send_scroll(character, message)
        room

      room.trainer.class_id && room.trainer.class_id != character.class_id ->
        message = "<p>This shop is not suitable for your training.</p>"
        Mobile.send_scroll(character, message)
        room

      character.level + 1 < room.trainer.min_level ->
        message = "<p>You have not progressed far enough to use the training provided here.</p>"
        Mobile.send_scroll(character, message)
        room

      character.level + 1 > room.trainer.max_level ->
        message = "<p>You have progressed too far to use the training provided here.</p>"
        Mobile.send_scroll(character, message)
        room

      character.level >= Character.max_level(character) ->
        message = "<p>You don't have the experience required to train!</p>"
        Mobile.send_scroll(character, message)
        room

      Trainer.training_cost(room.trainer, character) > Currency.wealth(character) ->
        message = "<p>You don't have the money required to train!</p>"
        Mobile.send_scroll(character, message)
        room

      :else ->
        Room.update_mobile(room, character.ref, fn character ->
          price_in_copper = Trainer.training_cost(room.trainer, character)
          currency = Currency.set_value(price_in_copper)
          char_currency = Currency.subtract(character, price_in_copper)

          old_abilities = Map.values(character.abilities)

          character =
            character
            |> Ecto.Changeset.change(%{
              level: character.level + 1,
              runic: char_currency.runic,
              platinum: char_currency.platinum,
              gold: char_currency.gold,
              silver: char_currency.silver,
              copper: char_currency.copper
            })
            |> Repo.update!()
            |> Character.load_abilities()
            |> Character.set_title()

          new_abilities = Map.values(character.abilities)

          Mobile.send_scroll(
            character,
            "<p>You hand over #{Currency.to_string(currency)}.</p>"
          )

          Mobile.send_scroll(
            character,
            "<p><span class='yellow'>Your level increases to #{character.level}!</span></p>"
          )

          Directory.add_character(%{
            name: character.name,
            bounty: character.bounty,
            room: character.room_id,
            ref: character.ref,
            title: character.title
          })

          Enum.each(new_abilities, fn ability ->
            unless ability in old_abilities do
              Mobile.send_scroll(
                character,
                "<p>\nYou've learned the <span class='dark-cyan'>#{ability.name}</span> ability!</p>"
              )

              Mobile.send_scroll(character, "<p>     #{ability.description}</p>")
            end
          end)

          character
        end)
    end
  end
end
