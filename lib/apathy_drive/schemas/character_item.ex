defmodule ApathyDrive.CharacterItem do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Character, Item}

  schema "characters_items" do
    field(:count, :integer, default: 0)
    belongs_to(:character, Character)
    belongs_to(:item, Item)
  end
end
