defmodule ApathyDrive.Class do
  use Ecto.Schema
  import Ecto.Changeset
  alias ApathyDrive.{Class, Repo}

  schema "classes" do
    field(:description, :string)
    field(:name, :string)

    has_many(:classes_traits, ApathyDrive.ClassTrait)
    has_many(:traits, through: [:classes_traits, :trait])

    timestamps()
  end

  @doc false
  def changeset(%Class{} = class, attrs \\ %{}) do
    class
    |> cast(attrs, [:name, :description])
    |> validate_required([:name, :description])
    |> cast_assoc(:classes_traits)
  end

  def select do
    Repo.all(__MODULE__, select: [:id, :name])
    |> Enum.map(&{&1.name, &1.id})
  end

  def all do
    Repo.all(__MODULE__, select: [:id, :name, :description])
  end

  def ids do
    Repo.all(__MODULE__, select: [:id])
    |> Enum.map(&Map.get(&1, :id))
  end
end
