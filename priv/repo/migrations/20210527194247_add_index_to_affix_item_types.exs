defmodule ApathyDrive.Repo.Migrations.AddIndexToAffixItemTypes do
  use Ecto.Migration

  def change do
    create(index(:affixes_items_types, [:affix_id, :item_type_id, :allowed]))
  end
end
