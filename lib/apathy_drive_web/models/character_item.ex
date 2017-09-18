defmodule ApathyDrive.CharacterItem do
  use ApathyDrive.Web, :model
  alias ApathyDrive.Item

  schema "characters_items" do
    field :level, :integer
    field :equipped, :boolean

    belongs_to :item, ApathyDrive.Item
    belongs_to :character, ApathyDrive.Character
  end

  def load_items(character_id) do
    __MODULE__
    |> where([ci], ci.character_id == ^character_id)
    |> preload(:item)
    |> Repo.all
    |> Enum.map(&Item.from_assoc/1)
  end

end
