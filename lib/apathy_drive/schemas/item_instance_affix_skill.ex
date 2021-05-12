defmodule ApathyDrive.ItemInstanceAffixSkill do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{AffixSkill, ItemInstance}

  schema "items_instances_affix_skills" do
    belongs_to(:item_instance, ItemInstance)
    belongs_to(:affix_skill, AffixSkill)

    field(:value, ApathyDrive.JSONB)
    field(:description, :string)
  end
end
