defmodule ApathyDrive.Repo.Migrations.RemoveUniqueTraitIndexes do
  use Ecto.Migration

  def change do
    drop(index(:items_traits, [:item_id, :trait_id], unique: true))
  end
end
