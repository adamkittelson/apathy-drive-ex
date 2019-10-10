defmodule ApathyDrive.ClassSpellcastingAttribute do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Class, Attribute}

  schema "classes_spellcasting_attributes" do
    belongs_to(:class, Class)
    belongs_to(:attribute, Attribute)
  end

  def changeset(model, params \\ %{}) do
    model
    |> cast(params, ~w(attribute_id class_id)a)
  end

  def load_attributes(class_id) do
    __MODULE__
    |> where([mt], mt.class_id == ^class_id)
    |> preload(:attribute)
    |> Repo.all()
    |> Enum.reduce([], fn %{attribute: %{name: attribute}}, attributes ->
      [attribute | attributes]
    end)
  end
end
