defmodule ApathyDrive.Enchantment do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{ItemInstance, Ability}

  schema "enchantments" do
    field :enchanted_by, :string
    field :progress, :float
    belongs_to :items_instances, ItemInstance
    belongs_to :ability, Ability
  end

end
