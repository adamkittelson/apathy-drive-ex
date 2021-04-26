defmodule ApathyDrive.AffixTrait do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Affix, Trait}

  schema "affixes_traits" do
    belongs_to(:affix, Affix)
    belongs_to(:trait, Trait)

    field(:value, ApathyDrive.JSONB)
    field(:description, :string)
  end
end
