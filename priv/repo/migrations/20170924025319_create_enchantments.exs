defmodule ApathyDrive.Repo.Migrations.CreateEnchantments do
  use Ecto.Migration

  def change do
    create table(:enchantments) do
      add :items_instances_id, references(:items_instances, on_delete: :delete_all)
      add :ability_id, references(:abilities, on_delete: :delete_all)
      add :enchanted_by, :text
      add :progress, :float, default: 0.0
    end

    create index(:enchantments, [:items_instances_id, :ability_id], unique: true)
  end
end
