defmodule ApathyDrive.Trait do
  use ApathyDrive.Web, :model

  schema "traits" do
    field :name, :string
    field :description, :string

    has_many :monsters_traits, ApathyDrive.MonsterTrait
    has_many :monsters, through: [:monsters_traits, :monster]

    timestamps()
  end

end
