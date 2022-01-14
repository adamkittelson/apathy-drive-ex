defmodule ApathyDrive.Commands.Train do
  use ApathyDrive.Command

  alias ApathyDrive.{
    Character,
    CharacterSkill,
    Match,
    Repo,
    Skill,
    Trainer,
    Trait
  }

  def keywords, do: ["train"]

  def execute(%Room{} = room, %Character{} = character, args, force \\ false) do
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
      level >= skill.max_level ->
        message = "<p>You cannot train #{skill.name} beyond level #{skill.max_level}.</p>"
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
              devs_spent: devs_spent
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
        |> Character.set_attribute_levels()

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
