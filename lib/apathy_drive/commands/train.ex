defmodule ApathyDrive.Commands.Train do
  use ApathyDrive.Command
  require Ecto.Query

  alias ApathyDrive.{
    Character,
    CharacterSkill,
    Currency,
    Directory,
    Level,
    Match,
    Repo,
    Skill,
    Trainer,
    Trait
  }

  def keywords, do: ["train"]

  def execute(room, character, args, force \\ false)

  def execute(%Room{} = room, %Character{} = character, [], _force) do
    modifier = (100 + character.race.race.exp_modifier) / 100
    level = character.level
    exp = trunc(character.experience)
    tolevel = Level.exp_at_level(level, modifier)
    remaining = tolevel - exp

    cond do
      !Trainer.trainer?(room) ->
        message = "<p>You must be in an appropriate training room to train!</p>"
        Mobile.send_scroll(character, message)
        room

      room.class_id && room.class_id != character.class_id ->
        guild = Trainer.guild_name(room)
        message = "<p>You must be a #{guild} to train here!</p>"
        Mobile.send_scroll(character, message)
        room

      remaining > 0 ->
        message = "<p>You don't have the experience required to train!</p>"
        Mobile.send_scroll(character, message)
        room

      Trainer.training_cost(character) > Currency.wealth(character) ->
        message = "<p>You don't have the money required to train!</p>"
        Mobile.send_scroll(character, message)
        room

      :else ->
        Room.update_mobile(room, character.ref, fn _room, character ->
          price_in_copper = Trainer.training_cost(character)
          currency = Currency.set_value(price_in_copper)
          char_currency = Currency.subtract(character, price_in_copper)

          character
          |> Ecto.assoc(:character_classes)
          |> Ecto.Query.where(class_id: ^character.class_id)
          |> Repo.one()
          |> Ecto.Changeset.change(%{
            level: character.level + 1
          })
          |> Repo.update!()

          character
          |> Ecto.assoc(:characters_skills)
          |> Repo.update_all(set: [current_level_times_trained: 0])

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
            |> Character.load_skills()
            |> Character.load_abilities()
            |> Character.set_title()
            |> Character.update_exp_bar()

          Mobile.send_scroll(
            character,
            "<p>You hand over #{Currency.to_string(currency)}.</p>"
          )

          Mobile.send_scroll(
            character,
            "<p>Your #{Trainer.guild_name(room)} level has increased to #{character.level}!</p>"
          )

          Directory.add_character(%{
            name: character.name,
            room: character.room_id,
            ref: character.ref,
            title: character.title
          })

          character
        end)
    end
  end

  def execute(%Room{} = room, %Character{} = character, args, force) do
    skill = Enum.join(args, " ")

    room.trainable_skills
    |> Enum.map(& &1.skill)
    |> Match.one(:keyword_starts_with, skill)
    |> case do
      %Skill{} = skill ->
        trainer = Enum.find(room.trainable_skills, &(&1.skill == skill))
        class = Trainer.guild_name(room)

        if trainer.class_id != character.class_id do
          message = "<p>You must be a #{class} to train here.</p>"
          Mobile.send_scroll(character, message)
          room
        else
          train(room, character, trainer, force)
        end

      nil ->
        message = "<p>You are unable to train that here.</p>"
        Mobile.send_scroll(character, message)
        room
    end
  end

  def train(room, character, %{skill: skill} = trainer, force) do
    level =
      CharacterSkill
      |> Repo.get_by(%{character_id: character.id, skill_id: skill.id})
      |> case do
        %CharacterSkill{} = character_skill ->
          character_skill.level

        nil ->
          0
      end

    dev_points = Character.development_points(character)
    cost = Trainer.dev_cost(character, skill, trainer.cost_modifier)
    module = Skill.module(skill.name)

    cond do
      level >= module.max_skill_level(character) ->
        message =
          "<p>You cannot train #{skill.name} beyond level #{module.max_skill_level(character)}.</p>"

        Mobile.send_scroll(character, message)
        room

      dev_points < cost and !force ->
        message = "<p>You do not have enough development points!</p>"
        Mobile.send_scroll(character, message)
        room

      character.level < skill.required_level ->
        message =
          "<p>You must be at least level #{skill.required_level} to train #{skill.name}!</p>"

        Mobile.send_scroll(character, message)
        room

      module.prereq() &&
          module.prereq().skill_level(character) < level ->
        message =
          "<p>You must have #{module.prereq().name()} fully trained before training #{skill.name}.</p>"

        Mobile.send_scroll(character, message)
        room

      :else ->
        train(room, character, skill.id, cost, trainer)
    end
  end

  def train(room, character, skill_id, devs_spent, trainer) do
    Room.update_mobile(room, character.ref, fn _room, character ->
      character_skill =
        CharacterSkill
        |> Repo.get_by(%{character_id: character.id, skill_id: skill_id})
        |> Repo.preload([:skill])
        |> case do
          %CharacterSkill{} = character_skill ->
            character_skill
            |> Ecto.Changeset.change(%{
              level: character_skill.level + 1,
              current_level_times_trained: character_skill.current_level_times_trained + 1,
              class_id: trainer.class_id,
              devs_spent: character_skill.devs_spent + devs_spent
            })
            |> Repo.update!()

          nil ->
            %CharacterSkill{
              character_id: character.id,
              skill_id: skill_id,
              level: 1,
              current_level_times_trained: 1,
              class_id: trainer.class_id,
              devs_spent: devs_spent
            }
            |> Repo.insert!()
            |> Repo.preload([:skill])
        end

      character =
        character
        |> Character.load_race()
        |> Character.load_classes()
        |> Character.add_equipped_items_effects()
        |> Character.load_skills()
        |> Character.load_abilities()
        |> Character.set_title()
        |> Character.update_exp_bar()

      level =
        case character_skill.skill.type do
          "skill" ->
            "#{character_skill.level}%"

          "ability" ->
            "*#{character_skill.level}"
        end

      Trait.bust_cache(character)

      Mobile.send_scroll(
        character,
        "<p>You spend #{devs_spent} to train #{String.downcase(character_skill.skill.name)} to #{level}.</p>"
      )

      cost = Trainer.dev_cost(character, character_skill.skill, trainer.cost_modifier)

      Mobile.send_scroll(
        character,
        "<p>It will cost you #{cost} development points to advance this #{character_skill.skill.type} further.</p>"
      )

      devs = Character.development_points(character)

      Mobile.send_scroll(
        character,
        "<p>You have #{devs} development points left.</p>"
      )

      character
    end)
  end
end
