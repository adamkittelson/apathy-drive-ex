defmodule ApathyDrive.ClassAttribute do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Attribute, Class}

  schema "classes_attributes" do
    field(:ratio, :integer)

    belongs_to(:attribute, Attribute)
    belongs_to(:class, Class)
  end

  def changeset(model, params \\ %{}) do
    model
    |> cast(params, ~w(attribute_id class_id ratio)a)
  end

  def load_ratios(class_id) do
    __MODULE__
    |> Ecto.Query.where(class_id: ^class_id)
    |> Ecto.Query.preload([:attribute])
    |> Repo.all()
  end
end
