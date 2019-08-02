defmodule ApathyDrive.Repo.Migrations.AddDropRarityToItems do
  use Ecto.Migration

  def change do
    alter table(:items) do
      add(:global_drop_rarity, :text)
    end
  end
end
