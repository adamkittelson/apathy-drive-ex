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

  def find(skill_name) do
    query = from s in Skill,
            where: s.name == ^skill_name,
            select: s

    Repo.one(query)
  end

end
