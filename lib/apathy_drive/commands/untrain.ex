defmodule ApathyDrive.Commands.Untrain do
  use ApathyDrive.Command

  alias ApathyDrive.{
    Character,
    CharacterClass,
    Class,
    Directory,
    Repo,
    Trainer
  }

  def keywords, do: ["untrain"]

  def execute(%Room{} = room, %Character{} = character, args) do
    args
    |> Enum.join(" ")
    |> Class.match_by_name()
    |> case do
      %Class{} = class ->
        untrain(room, character, class.id)

      nil ->
        message = "<p>You must supply a valid class to untrain, e.g. untrain paladin</p>"
        Mobile.send_scroll(character, message)
        room
    end
  end

  def untrain(room, character, class_id) do
    if !Trainer.trainer?(room) or !room.trainer_id do
      message = "<p>You must be in an appropriate training room to untrain!</p>"
      Mobile.send_scroll(character, message)
      room
    else
      class = Repo.get(Class, class_id).name

      Room.update_mobile(room, character.ref, fn _room, character ->
        old_abilities = Map.values(character.abilities)
        old_hp = Mobile.max_hp_at_level(character, character.level)
        old_exp = Character.trainable_experience(character)

        CharacterClass
        |> Repo.get_by(%{character_id: character.id, class_id: class_id})
        |> Repo.preload([:class])
        |> case do
          %CharacterClass{level: 0} ->
            Mobile.send_scroll(
              character,
              "<p>You do not have any #{class} levels to untrain!</p>"
            )

            room

          %CharacterClass{} = character_class ->
            character_class =
              character_class
              |> Ecto.Changeset.change(%{
                level: character_class.level - 1
              })
              |> Repo.update!()

            if character_class.level == 0 do
              Repo.delete!(character_class)
            end

            character =
              character
              |> Map.put(:effects, %{})
              |> Character.load_classes()
              |> Character.load_race()
              |> Character.set_skill_levels()
              |> Character.add_equipped_items_effects()
              |> Character.load_abilities()
              |> Character.set_title()
              |> Character.update_exp_bar()

            new_exp = Character.trainable_experience(character)

            character =
              character
              |> Ecto.Changeset.change(%{
                experience: character.experience - (new_exp - old_exp)
              })
              |> Repo.update!()

            new_abilities = Map.values(character.abilities)

            Mobile.send_scroll(
              character,
              "<p><span class='yellow'>Your #{class} level has decreased to #{
                character_class.level
              }!</span></p>"
            )

            hp_diff = Mobile.max_hp_at_level(character, character.level) - old_hp

            if hp_diff != 0 do
              Mobile.send_scroll(
                character,
                "<p><span class='yellow'>Your maximum health is decreased by #{abs(hp_diff)}.</span></p>"
              )
            end

            Directory.add_character(%{
              name: character.name,
              evil_points: character.evil_points,
              room: character.room_id,
              ref: character.ref,
              title: character.title
            })

            Enum.each(old_abilities, fn ability ->
              unless ability in new_abilities do
                Mobile.send_scroll(
                  character,
                  "<p>\nYou've forgot the <span class='dark-cyan'>#{ability.name}</span> ability!</p>"
                )

                Mobile.send_scroll(character, "<p>     #{ability.description}</p>")
              end
            end)

            character

          nil ->
            Mobile.send_scroll(
              character,
              "<p>You do not have any #{class} levels to untrain!</p>"
            )

            room
        end
      end)
    end
  end
end
