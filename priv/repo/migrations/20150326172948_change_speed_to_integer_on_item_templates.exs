defmodule ApathyDrive.Repo.Migrations.ChangeSpeedToIntegerOnItemTemplates do
  use Ecto.Migration

  def up do
    alter table(:item_templates) do
      modify :speed, :integer
    end
  end

  def down do
    alter table(:item_templates) do
      modify :speed, :float
    end
  end
end
