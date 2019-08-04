defmodule ApathyDrive.CharacterStyle do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Character, Item}

  schema "characters_styles" do
    belongs_to(:character, Character)
    belongs_to(:item, Item)
  end

  def for_character(%Character{} = character) do
    __MODULE__
    |> where([r], r.character_id == ^character.id)
    |> preload(:item)
  end
end
