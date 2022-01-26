defmodule ApathyDrive.Skill do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Character, CharacterSkill, Match, Mobile, Skill}

  schema "skills" do
    field(:name, :string)
    field(:command, :string)
    field(:required_level, :integer)
    field(:max_level, :integer)
    field(:dev_cost, :integer)
    field(:fast_dev_cost, :integer)
    field(:type, :string)

    has_many(:characters_skills, CharacterSkill)
    has_many(:characters, through: [:characters_skills, :character])
    belongs_to(:casting_skill, Skill)
  end

  defmacro __using__(_opts) do
    quote do
      alias ApathyDrive.{Repo, Skill}

      def prereq(), do: nil

      def name() do
        __MODULE__
        |> to_string()
        |> String.split(".")
        |> List.last()
        |> String.replace(~r/([A-Z])/, " \\1")
        |> String.trim()
      end

      def base_skill_level(character) do
        character.skills
        |> Map.values()
        |> List.flatten()
        |> Enum.find(&(&1.name == name()))
        |> case do
          %{level: level} ->
            level

          _ ->
            0
        end
      end

      def skill_level(character) do
        character.skills
        |> Map.values()
        |> List.flatten()
        |> Enum.find(&(&1.name == name()))
        |> case do
          %{attributes: attributes, skill: %{type: "skill"}, level: base} = skill ->
            total =
              attributes
              |> Enum.map(
                &Mobile.attribute_at_level(
                  character,
                  String.to_existing_atom(&1),
                  character.level
                )
              )
              |> Enum.sum()

            average = total / length(attributes)

            with_charm =
              (average * 5 + Mobile.attribute_at_level(character, :charm, character.level)) / 6

            trunc(with_charm + base)

          %{level: base} = skill ->
            base

          nil ->
            0
        end
      end

      def current_level_times_trained(character) do
        character.skills
        |> Map.values()
        |> List.flatten()
        |> Enum.find(&(&1.name == name()))
        |> case do
          %{current_level_times_trained: times} ->
            times

          _ ->
            0
        end
      end

      def max_skill_level(character) do
        character.skills
        |> Map.values()
        |> List.flatten()
        |> Enum.find(&(&1.name == name()))
        |> case do
          %{max_level: level} ->
            level

          _ ->
            Repo.get_by(Skill, name: name()).max_level
        end
      end

      def prereq(character, level) do
        if prereq() && prereq().skill_level(character) < level do
          "<span class='red'>Prerequisite: #{prereq().name()} Rank #{level}</span>\n"
        end
      end

      def required_level(level) do
        req = Repo.get_by(Skill, name: name()).required_level

        if level < req do
          "<span class='red'>Required Character Level: #{req}</span>\n"
        end
      end

      def attributes() do
        ability(%Character{level: 1}).attributes
        |> Enum.map(&String.capitalize/1)
        |> Enum.join(", ")
      end

      defoverridable(prereq: 0)
    end
  end

  def module(skill_name) do
    module_name =
      skill_name
      |> String.split(~r/[^\w]+/)
      |> Enum.map(&Macro.camelize/1)
      |> Enum.join()

    Module.concat([ApathyDrive, Skills, module_name])
  end

  def create_changeset(name) do
    %__MODULE__{}
    |> cast(%{name: name}, ~w(name))
    |> validate_required(:name)
    |> validate_format(:name, ~r/^[a-zA-Z\d ,\-']+$/)
    |> validate_length(:name, min: 1, max: 20)
    |> unique_constraint(:name)
  end

  def match_by_name(name, all \\ false) do
    skills =
      __MODULE__
      |> where([skill], not is_nil(skill.name) and skill.name != "")
      |> distinct(true)
      |> select([area], [:id, :name])
      |> ApathyDrive.Repo.all()

    if all do
      Match.all(skills, :keyword_starts_with, name)
    else
      Match.one(skills, :keyword_starts_with, name)
    end
  end
end
