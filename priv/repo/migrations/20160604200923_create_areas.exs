defmodule ApathyDrive.Repo.Migrations.CreateAreas do
  use Ecto.Migration

  def change do
    create table(:areas) do
      add(:name, :text)
      add(:level, :integer)

      timestamps()
    end

    alter table(:rooms) do
      add(:area_id, :integer)
    end

    create(index(:rooms, [:area_id]))
  end
end
