defmodule ApathyDrive.Repo.Migrations.AddWeightToItemTemplates do
  use Ecto.Migration

  def up do
    alter table(:item_templates) do
      add :weight, :integer
    end
  end

  def down do
    alter table(:item_templates) do
      remove :weight
    end
  end
end
