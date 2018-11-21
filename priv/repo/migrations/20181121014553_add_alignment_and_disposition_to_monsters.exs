defmodule ApathyDrive.Repo.Migrations.AddAlignmentAndDispositionToMonsters do
  use Ecto.Migration

  def change do
    alter table(:monsters) do
      add(:alignment, :text)
      add(:disposition, :text)
    end
  end
end
