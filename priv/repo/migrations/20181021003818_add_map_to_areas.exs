defmodule ApathyDrive.Repo.Migrations.AddMapToAreas do
  use Ecto.Migration

  def change do
    alter table(:areas) do
      add(:map, :jsonb)
    end
  end
end
