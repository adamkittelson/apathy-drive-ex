defmodule ApathyDrive.Repo.Migrations.AddLoreToCharacters do
  use Ecto.Migration

  def change do
    alter table(:characters) do
      add(:lore, :text)
    end
  end
end
