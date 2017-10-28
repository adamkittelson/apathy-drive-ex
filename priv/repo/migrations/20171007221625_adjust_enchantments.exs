defmodule ApathyDrive.Repo.Migrations.AdjustEnchantments do
  use Ecto.Migration

  def change do
    alter table(:enchantments) do
      add :finished, :boolean, default: false
      add :time_elapsed_in_seconds, :integer, default: 0
      remove :enchanted_by
      remove :progress
    end
  end
end
