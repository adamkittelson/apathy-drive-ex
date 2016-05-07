defmodule ApathyDrive.Herp do
  use ApathyDrive.Web, :model

  schema "herps" do
    field :derp, :string

    timestamps
  end

  @doc """
  Builds a changeset based on the `struct` and `params`.
  """
  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:derp])
    |> validate_required([:derp])
  end
end
