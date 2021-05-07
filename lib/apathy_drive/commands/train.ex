defmodule ApathyDrive.Commands.Train do
  use ApathyDrive.Command

  alias ApathyDrive.{
    Character,
    CharacterClass,
    CharacterSkill,
    Class,
    Level,
    Match,
    Repo,
    Skill
  }

  def keywords, do: ["train"]

  def execute(%Room{} = room, %Character{} = character, args, force \\ false) do
    skill = Enum.join(args, " ")

    room.trainable_skills
    |> Match.one(:keyword_starts_with, skill)
    |> case do
      %Skill{} = skill ->
        train(room, character, skill, force)

      nil ->
        message = "<p>You are unable to train that here.</p>"
        Mobile.send_scroll(character, message)
        room
    end
  end

  def train(room, character, %Skill{} = skill, force) do
    level =
      CharacterSkill
      |> Repo.get_by(%{character_id: character.id, skill_id: skill.id})
      |> case do
        %CharacterSkill{} = character_skill ->
          character_skill.level

        nil ->
          0
      end

    skill_points = Character.skill_points(character)

    cond do
      level >= 20 ->
        message = "<p>You cannot train #{skill.name} beyond level 20.</p>"
        Mobile.send_scroll(character, message)
        room

      skill_points < 1 and !force ->
        message = "<p>You do not have any availabe skill points!</p>"
        Mobile.send_scroll(character, message)
        room

      :else ->
        train(room, character, skill.id)
    end
  end

  def train(room, character, skill_id) do
    Room.update_mobile(room, character.ref, fn _room, character ->
      character_skill =
        CharacterSkill
        |> Repo.get_by(%{character_id: character.id, skill_id: skill_id})
        |> Repo.preload([:skill])
        |> case do
          %CharacterSkill{} = character_skill ->
            character_skill
            |> Ecto.Changeset.change(%{
              level: character_skill.level + 1
            })
            |> Repo.update!()

          nil ->
            %CharacterSkill{character_id: character.id, skill_id: skill_id, level: 1}
            |> Repo.insert!()
            |> Repo.preload([:skill])
        end

      character =
        character
        |> Character.load_classes()
        |> Character.load_race()
        |> Character.add_equipped_items_effects()
        |> Character.load_skills()
        |> Character.load_abilities()
        |> Character.set_title()
        |> Character.update_exp_bar()

      Mobile.send_scroll(
        character,
        "<p><span class='yellow'>Your #{character_skill.skill.name} level has increased to #{
          character_skill.level
        }!</span></p>"
      )

      character
    end)
  end
end
