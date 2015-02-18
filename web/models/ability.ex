defmodule Ability do
  use Ecto.Model
  alias ApathyDrive.Repo

  schema "abilities" do
    field :name,            :string
    field :command,         :string
    field :description,     :string
    field :required_skills, ApathyDrive.JSONB
    field :properties,      ApathyDrive.JSONB
    field :keywords,        {:array, :string}, virtual: true

    timestamps
  end

  after_load :set_keywords

  def set_keywords(%Ability{name: name} = ability) do
    Map.put(ability, :keywords, String.split(name))
  end

end
