defmodule ApathyDrive.Attribute do
  use ApathyDriveWeb, :model

  schema "attributes" do
    field(:name, :string)
    field(:description, :string)
  end

  def changeset(model, params \\ %{}) do
    model
    |> cast(params, ~w(name description)a)
  end
end
