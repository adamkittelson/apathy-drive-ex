defmodule ApathyDrive.Area do
  use ApathyDrive.Web, :model
  alias ApathyDrive.Area

  schema "areas" do
    field :name, :string
    field :level, :integer, default: 1

    has_many :rooms, ApathyDrive.Room

    timestamps
  end

  def find_by_name(name) do
    __MODULE__
    |> where(name: ^name)
  end

  def changeset(name) do
    %__MODULE__{}
    |> cast(%{name: name}, ~w(name))
    |> validate_required(:name)
    |> validate_format(:name, ~r/^[a-zA-Z ,]+$/)
    |> validate_length(:name, min: 1, max: 20)
    |> unique_constraint(:name)
  end

  def update_level(%Area{} = area, level) when is_integer(level) do
    area
    |> Map.put(:level, level)
    |> Repo.save!
  end
  def update_level(area, level), do: update_level(area, String.to_integer(level))

end
