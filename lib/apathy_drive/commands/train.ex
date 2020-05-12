defmodule ApathyDrive.Commands.Train do
  use ApathyDrive.Command

  alias ApathyDrive.{
    Character,
    CharacterClass,
    ClassSkill,
    Directory,
    Level,
    Repo,
    Trainer
  }

  def keywords, do: ["train"]

  def execute(%Room{} = room, %Character{} = character, args) do
    if !Trainer.trainer?(room) || !room.trainer_id || !room.trainer.class_id do
      message = "<p>You must be in an appropriate training room to train!</p>"
      Mobile.send_scroll(character, message)
      room
    else
      train(room, character, room.trainer.class_id, args)
    end
  end

  def required_experience(character, class_id, level \\ nil) do
    initial_exp = Character.used_experience(character)

    classes =
      if index = Enum.find_index(character.classes, &(&1.class_id == class_id)) do
        class = Enum.at(character.classes, index)
        class = update_in(class.level, &(level || &1 + 1))
        List.replace_at(character.classes, index, class)
      else
        [%CharacterClass{level: level || 1, class_id: class_id} | character.classes]
      end

    new_exp =
      character
      |> Map.put(:classes, classes)
      |> Character.used_experience()

    new_exp - initial_exp
  end

  def train(room, character, class_id, force) do
    required_exp = required_experience(character, class_id)

    if Character.trainable_experience(character) >= required_exp or force == true do
      Room.update_mobile(room, character.ref, fn _room, character ->
        old_abilities = Map.values(character.abilities)
        old_hp = Mobile.max_hp_at_level(character, character.level)

        character_class =
          CharacterClass
          |> Repo.get_by(%{character_id: character.id, class_id: class_id})
          |> Repo.preload([:class])
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
              |> Repo.preload([:class])
          end

        character =
          character
          |> Character.load_classes()
          |> Character.load_race()
          |> Character.add_equipped_items_effects()
          |> Character.load_abilities()
          |> Character.set_title()
          |> Character.update_exp_bar()

        new_abilities = Map.values(character.abilities)

        Mobile.send_scroll(
          character,
          "<p><span class='yellow'>Your #{character_class.class.name} level has increased to #{
            character_class.level
          }!</span></p>"
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

        character =
          class_id
          |> ClassSkill.load_skills()
          |> Enum.reduce(character, fn skill_name, character ->
            skill = character.skills[skill_name]
            exp = skill.experience
            level = skill.level

            to_level = Level.exp_at_level(level + 1, 1.0)

            Character.add_skill_experience(character, skill_name, to_level - exp)
          end)

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
    else
      message = "<p>You don't have the #{required_exp} required experience to train.</p>"
      Mobile.send_scroll(character, message)
      room
    end
  end
end
