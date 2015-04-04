defmodule ApathyDrive.Repo.Migrations.AddPropertiesToItemTemplates do
  use Ecto.Migration

  def up do
    alter table(:item_templates) do
      add :properties, :jsonb
      remove :damage
    end
  end

  def down do
    alter table(:item_templates) do
      add :damage, :jsonb
      remove :properties
    end
  end
end
