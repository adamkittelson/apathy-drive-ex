defmodule ApathyDrive.CharacterReputation do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Area, Character}

  schema "characters_reputations" do
    belongs_to :character, Character
    belongs_to :area, Area
    field :reputation, :float, default: 0.0
  end

end
