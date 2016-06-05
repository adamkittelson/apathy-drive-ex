defmodule ApathyDrive.Area do
  use ApathyDrive.Web, :model

  schema "areas" do
    field :name, :string
    field :level, :integer

    has_many :rooms, ApathyDrive.Room

    timestamps
  end

  def find_by_name(name) do
    __MODULE__
    |> where(name: ^name)
  end

end
