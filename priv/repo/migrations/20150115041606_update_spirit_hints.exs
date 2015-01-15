defmodule ApathyDrive.Repo.Migrations.UpdateSpiritHints do
  use Ecto.Migration

  def up do
    ["ALTER TABLE spirits DROP COLUMN hints",
     "ALTER TABLE spirits ADD COLUMN hints text[]",
     "ALTER TABLE spirits ADD COLUMN disabled_hints text[]"]
  end

  def down do
    ["ALTER TABLE spirits DROP COLUMN hints",
     "ALTER TABLE spirits ADD COLUMN hints jsonb",
     "ALTER TABLE spirits DROP COLUMN disabled_hints"]
  end
end
