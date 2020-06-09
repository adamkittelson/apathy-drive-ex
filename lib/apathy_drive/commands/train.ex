defmodule ApathyDrive.Commands.Train do
  use ApathyDrive.Command

  alias ApathyDrive.{
    Character,
    CharacterClass,
    Class,
    Currency,
    Directory,
    Repo,
    Trainer
  }

  def keywords, do: ["train"]

  def execute(%Room{} = room, %Character{} = character, args, force \\ false) do
    args
    |> Enum.join(" ")
    |> Class.match_by_name()
    |> case do
      %Class{} = class ->
        train(room, character, class, force)

      nil ->
        message = "<p>You must supply a valid class to train, e.g. train paladin</p>"
        Mobile.send_scroll(character, message)
        room
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

  def train(room, character, class, force \\ false)

  def train(room, character, %Class{} = class, force) do
    level =
      CharacterClass
      |> Repo.get_by(%{character_id: character.id, class_id: class.id})
      |> case do
        %CharacterClass{} = character_class ->
          character_class.level

        nil ->
          0
      end

    required_exp = required_experience(character, class.id)

    cond do
      !Trainer.trainer?(room) or !room.trainer_id ->
        message = "<p>You must be in an appropriate training room to train!</p>"
        Mobile.send_scroll(character, message)
        room

      !is_nil(room.trainer.class_id) and room.trainer.class_id != class.id and !force ->
        message = "<p>You may not train that class here!</p>"
        Mobile.send_scroll(character, message)
        room

      level + 1 < room.trainer.min_level and !force ->
        message = "<p>You have not progressed far enough to use the training provided here.</p>"
        Mobile.send_scroll(character, message)
        room

      level + 1 > room.trainer.max_level and !force ->
        message = "<p>You have progressed too far to use the training provided here.</p>"
        Mobile.send_scroll(character, message)
        room

      Character.trainable_experience(character) < required_exp and !force ->
        message = "<p>You don't have the #{required_exp} required experience to train.</p>"
        Mobile.send_scroll(character, message)
        room

      Trainer.training_cost(room.trainer, character) > Currency.wealth(character) and !force ->
        message = "<p>You don't have the money required to train!</p>"
        Mobile.send_scroll(character, message)
        room

      :else ->
        train(room, character, class.id)
    end
  end

  def train(room, character, class_id, force) do
    Room.update_mobile(room, character.ref, fn _room, character ->
      price_in_copper =
        if force do
          0
        else
          Trainer.training_cost(room.trainer, character)
        end

      currency = Currency.set_value(price_in_copper)
      char_currency = Currency.subtract(character, price_in_copper)

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
        |> Ecto.Changeset.change(%{
          runic: char_currency.runic,
          platinum: char_currency.platinum,
          gold: char_currency.gold,
          silver: char_currency.silver,
          copper: char_currency.copper
        })
        |> Repo.update!()
        |> Character.load_classes()
        |> Character.load_race()
        |> Character.set_skill_levels()
        |> Character.add_equipped_items_effects()
        |> Character.load_abilities()
        |> Character.set_title()
        |> Character.update_exp_bar()

      Mobile.send_scroll(
        character,
        "<p>You hand over #{Currency.to_string(currency)}.</p>"
      )

      new_abilities = Map.values(character.abilities)

      Mobile.send_scroll(
        character,
        "<p><span class='yellow'>Your #{character_class.class.name} level has increased to #{
          character_class.level
        }!</span></p>"
      )

      hp_diff = Mobile.max_hp_at_level(character, character.level) - old_hp

      if hp_diff != 0 do
        Mobile.send_scroll(
          character,
          "<p><span class='yellow'>Your maximum health is increased by #{hp_diff}.</span></p>"
        )
      end

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
