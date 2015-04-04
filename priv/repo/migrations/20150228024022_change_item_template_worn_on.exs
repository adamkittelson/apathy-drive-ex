defmodule ApathyDrive.Repo.Migrations.ChangeItemTemplateWornOn do
  use Ecto.Migration

  def up do
    alter table(:item_templates) do
      modify :worn_on, :text
      remove :slot
    end
  end

  def down do
    alter table(:item_templates) do
      modify :worn_on, :jsonb
      add :slot, :text
    end
  end
end
