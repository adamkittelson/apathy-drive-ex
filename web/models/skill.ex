defmodule Skill do
  use Ecto.Model
  alias ApathyDrive.Repo

  schema "skills" do
    field :name,         :string
    field :level,        :integer, default: 1
    field :cost,         :float
    field :universal,    :boolean, default: true
    field :description,  :string
    field :keywords,     {:array, :string}, virtual: true

    timestamps
  end

  after_load :set_keywords

  def set_keywords(%Skill{name: name} = skill) do
    Map.put(skill, :keywords, String.split(name))
  end

  def insert(%Skill{id: nil} = skill) do
    Repo.insert(skill)
  end

  def all do
    Repo.all from s in Skill, select: s
  end

  def all(%Room{trainable_skills: nil}) do
    universal
  end

  def all(%Room{trainable_skills: trainable_skills}) do
    universal ++ Enum.map(trainable_skills, &(find(&1)))
  end

  def universal do
    Repo.all from s in Skill, where: s.universal == true, select: s
  end

  def with_modifier(attribute) when is_binary(attribute) do
    attribute |> String.to_atom |> with_modifier
  end
  def with_modifier(attribute) do
    Repo.all from s in Skill, where: field(s, ^attribute) > 0, select: s
  end

  def with_modifier(attribute, skill_names) when is_binary(attribute) do
    attribute |> String.to_atom |> with_modifier(skill_names)
  end
  def with_modifier(attribute, skill_names) do
    Repo.all from s in Skill, where: field(s, ^attribute) > 0 and s.name in ^skill_names, select: s
  end

  def find(skill_name) do
    query = from s in Skill,
            where: s.name == ^skill_name,
            select: s

    Repo.one(query)
  end

end
