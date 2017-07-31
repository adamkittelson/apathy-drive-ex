defmodule ApathyDrive.EntitySpell do
  use ApathyDrive.Web, :model

  schema "entities_spells" do
    field :assoc_table, :string
    field :assoc_id, :integer
    field :level, :integer
    belongs_to :spell, ApathyDrive.Spell

    timestamps
  end

end
