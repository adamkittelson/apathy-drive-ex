defmodule ApathyDrive.Repo.Migrations.RemoveLightDurationFromItemTemplates do
  use Ecto.Migration

  def up do
    alter table(:item_templates) do
      remove :light_duration
    end
  end

  def down do
    alter table(:item_templates) do
      add :light_duration, :integer
    end
  end
end
