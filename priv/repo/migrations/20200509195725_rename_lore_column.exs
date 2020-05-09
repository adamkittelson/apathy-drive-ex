defmodule ApathyDrive.Repo.Migrations.RenameLoreColumn do
  use Ecto.Migration

  def change do
    rename(table(:characters), :lore, to: :lore_name)
  end
end
