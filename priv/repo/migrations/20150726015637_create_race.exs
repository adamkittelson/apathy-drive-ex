defmodule ApathyDrive.Repo.Migrations.CreateRace do
  use Ecto.Migration

  def change do
    create table(:races) do
      add(:name, :text)
      add(:description, :text)
      add(:properties, :jsonb)

      timestamps()
    end
  end
end
