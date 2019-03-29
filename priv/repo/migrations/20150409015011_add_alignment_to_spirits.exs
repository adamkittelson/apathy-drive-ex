defmodule ApathyDrive.Repo.Migrations.AddAlignmentToSpirits do
  use Ecto.Migration

  def change do
    alter table(:spirits) do
      add(:alignment, :text)
    end
  end
end
