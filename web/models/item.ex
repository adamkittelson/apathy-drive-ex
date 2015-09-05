defmodule ApathyDrive.Item do
  use ApathyDrive.Web, :model

  schema "items" do
    field :name, :string
    field :description, :string
    field :weight, :integer
    field :worn_on, :string
    field :physical_defense, :integer
    field :magical_defense, :integer
    field :level, :integer
    field :strength, :integer
    field :agility, :integer
    field :will, :integer
    field :grade, :integer

    timestamps
  end

  @required_fields ~w(name description weight worn_on physical_defense magical_defense level strength agility will grade)
  @optional_fields ~w()

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  def changeset(model, params \\ :empty) do
    model
    |> cast(params, @required_fields, @optional_fields)
  end
end
