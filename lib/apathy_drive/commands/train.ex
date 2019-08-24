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
        change_class(room, character, room.trainer.class_id)

      character.level + 1 < room.trainer.min_level ->
        message = "<p>You have not progressed far enough to use the training provided here.</p>"
        Mobile.send_scroll(character, message)
        room

      character.level + 1 > room.trainer.max_level ->
        message = "<p>You have progressed too far to use the training provided here.</p>"
        Mobile.send_scroll(character, message)
        room

      character.level > Character.max_level(character) ->
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
          old_hp = Mobile.max_hp_at_level(character, character.level)

          character =
            update_in(character.class, fn character_class ->
              character_class
              |> Ecto.Changeset.change(%{
                level: character_class.level + 1
              })
              |> Repo.update!()
            end)

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
            |> Character.load_class()
            |> Character.load_race()
            |> Character.load_items()
            |> Character.load_abilities()
            |> Character.set_title()
            |> Character.update_exp_bar()

          new_abilities = Map.values(character.abilities)

          Mobile.send_scroll(
            character,
            "<p>You hand over #{Currency.to_string(currency)}.</p>"
          )

          Mobile.send_scroll(
            character,
            "<p>Your level has increased to #{character.level}!</p>"
          )

          hp_diff = Mobile.max_hp_at_level(character, character.level) - old_hp

          Mobile.send_scroll(
            character,
            "<p>Your maximum health is increased by #{hp_diff}.</p>"
          )

          Directory.add_character(%{
            name: character.name,
            evil_points: character.evil_points,
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

  def change_class(room, character, class_id) do
    Room.update_mobile(room, character.ref, fn character ->
      character =
        character
        |> Ecto.Changeset.change(%{
          class_id: class_id
        })
        |> Repo.update!()
        |> Character.load_class()
        |> Character.load_race()
        |> Character.load_items()
        |> Character.load_abilities()
        |> Character.set_title()
        |> Character.update_exp_bar()

      Directory.add_character(%{
        name: character.name,
        evil_points: character.evil_points,
        room: character.room_id,
        ref: character.ref,
        title: character.title
      })

      message = "<p>You are now a #{character.class.class.name}.</p>"

      Mobile.send_scroll(character, message)
    end)
  end
end
