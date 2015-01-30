defmodule ApathyDrive.Repo.Migrations.RenameValueToCostOnItemTeplates do
  use Ecto.Migration

  def up do
    "ALTER TABLE item_templates RENAME COLUMN value TO cost"
  end

  def down do
    "ALTER TABLE item_templates RENAME COLUMN cost TO value"
  end
end
