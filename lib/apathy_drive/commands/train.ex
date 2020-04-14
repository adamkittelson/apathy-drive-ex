defmodule ApathyDrive.Commands.Train do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, CharacterClass, Directory, Repo, Trainer}

  def keywords, do: ["train"]

  def execute(%Room{} = room, %Character{} = character, _args) do
    cond do
      !Trainer.trainer?(room) || !room.trainer_id || !room.trainer.class_id ->
        message = "<p>You must be in an appropriate training room to train!</p>"
        Mobile.send_scroll(character, message)
        room

      room.trainer.class_id && character.level < Character.max_level(character) ->
        train(room, character, room.trainer.class_id)

      :else ->
        message = "<p>You don't have the required experience to train.</p>"
        Mobile.send_scroll(character, message)
        room
    end
  end

  def train(room, character, class_id) do
    Room.update_mobile(room, character.ref, fn _room, character ->
      old_abilities = Map.values(character.abilities)
      old_hp = Mobile.max_hp_at_level(character, character.level)

      CharacterClass
      |> Repo.get_by(%{character_id: character.id, class_id: class_id})
      |> case do
        %CharacterClass{} = character_class ->
          character_class
          |> Ecto.Changeset.change(%{
            level: character_class.level + 1
          })
          |> Repo.update!()

        nil ->
          %CharacterClass{character_id: character.id, class_id: class_id, level: 1}
          |> Repo.insert!()
      end

      character =
        character
        |> Character.load_classes()
        |> Character.load_race()
        |> Character.load_abilities()
        |> Character.set_title()
        |> Character.update_exp_bar()

      new_abilities = Map.values(character.abilities)

      Mobile.send_scroll(
        character,
        "<p><span class='yellow'>Your level has increased to #{character.level}!</span></p>"
      )

      hp_diff = Mobile.max_hp_at_level(character, character.level) - old_hp

      Mobile.send_scroll(
        character,
        "<p><span class='yellow'>Your maximum health is increased by #{hp_diff}.</span></p>"
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
