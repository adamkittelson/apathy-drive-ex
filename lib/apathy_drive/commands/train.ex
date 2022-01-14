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
    |> Enum.find(
      &(Match.keyword_starts_with(skill, &1.skill) and &1.class_id == character.class_id)
    )
    |> case do
      %{} = trainer ->
        train(room, character, trainer, force)

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
        train(room, character, skill.id, cost, trainer.class_id)
    end
  end

  def train(room, character, skill_id, devs_spent, class_id) do
    Room.update_mobile(room, character.ref, fn _room, character ->
      attribute_levels = attribute_levels(character)

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
              class_id: class_id,
              devs_spent: devs_spent
            })
            |> Repo.update!()

          nil ->
            %CharacterSkill{
              character_id: character.id,
              skill_id: skill_id,
              level: 1,
              current_level_times_trained: 1,
              class_id: class_id,
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

      Mobile.send_scroll(
        character,
        "<p><span class='yellow'>Your #{character_skill.skill.name} level has increased to #{character_skill.level}!</span></p>"
      )

      Trait.bust_cache(character)

      updated_attribute_levels = attribute_levels(character)

      updated_attribute_levels
      |> Enum.each(fn {attribute, level} ->
        if level > attribute_levels[attribute] do
          message = "<p><span class='yellow'>Your #{attribute} increased to #{level}!</span></p>"

          Mobile.send_scroll(character, message)
        end
      end)

      ApathyDrive.Commands.Help.execute(room, character, [character_skill.skill.name])

      character
    end)
  end

  defp attribute_levels(character) do
    [:strength, :agility, :intellect, :willpower, :health, :charm]
    |> Enum.reduce(%{}, fn attribute, levels ->
      Map.put(levels, attribute, Mobile.attribute_at_level(character, attribute, character.level))
    end)
  end
end
