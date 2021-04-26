defmodule ApathyDrive.ItemInstanceAffixTrait do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{AffixTrait, ItemInstance}

  schema "items_instances_affixes_traits" do
    belongs_to(:item_instance, ItemInstance)
    belongs_to(:affix_traits, AffixTrait)

    field(:value, ApathyDrive.JSONB)
    field(:description, :string)
  end
end
