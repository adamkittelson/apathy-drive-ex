defmodule ApathyDrive.Class do
  use Ecto.Schema
  import Ecto.Changeset
  alias ApathyDrive.Class


  schema "classes" do
    field :description, :string
    field :name, :string

    timestamps()
  end

  @doc false
  def changeset(%Class{} = class, attrs) do
    class
    |> cast(attrs, [:name, :description])
    |> validate_required([:name, :description])
  end
end
