defmodule ApathyDrive.Skill do
  use ApathyDrive.Web, :model
  alias ApathyDrive.Match

  schema "skills" do
    field :name, :string
    field :training_cost_multiplier, :float, default: 1.0
    field :description, :string

    field :experience, :integer, virtual: true, default: 0
    field :level, :integer, virtual: true, default: 0

    has_many :skills_incompatibilities, ApathyDrive.SkillIncompatibility
    has_many :incompatible_skills, through: [:skills_incompatibilities, :incompatible_skill]
    has_many :rooms_skills, ApathyDrive.RoomSkill
    has_many :trainers, through: [:rooms_skills, :room]

    has_many :characters_skills, ApathyDrive.CharacterSkill
    has_many :characters, through: [:characters_skills, :character]
  end

  def set_level(%__MODULE__{experience: exp, training_cost_multiplier: multiplier} = skill) when not is_nil(exp) do
    put_in(skill.level, ApathyDrive.Level.skill_level_at_exp(exp, multiplier))
  end

  def create_changeset(name) do
    %__MODULE__{}
    |> cast(%{name: name}, ~w(name))
    |> validate_required(:name)
    |> validate_format(:name, ~r/^[a-zA-Z\d ,\-']+$/)
    |> validate_length(:name, min: 1, max: 20)
    |> unique_constraint(:name)
  end

  def set_cost_changeset(%__MODULE__{} = skill, cost) do
    skill
    |> cast(%{training_cost_multiplier: cost}, ~w(training_cost_multiplier))
    |> validate_required(:training_cost_multiplier)
    |> validate_number(:training_cost_multiplier, [greater_than: 0])
  end

  def match_by_name(name, all \\ false) do
    skills =
      __MODULE__
      |> where([skill], not is_nil(skill.name) and skill.name != "")
      |> distinct(true)
      |> select([area], [:id, :name, :training_cost_multiplier])
      |> ApathyDrive.Repo.all

    if all do
      Match.all(skills, :keyword_starts_with, name)
    else
      Match.one(skills, :keyword_starts_with, name)
    end
  end

end
