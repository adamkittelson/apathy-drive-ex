defmodule ApathyDrive.Repo.Migrations.CreateSpirits do
  use Ecto.Migration

  def up do
    create table(:spirits) do
      add(:name, :text)
      add(:experience, :integer)
      add(:level, :integer)
      add(:url, :text)
      add(:hints, {:array, :string})
      add(:disabled_hints, {:array, :string})
      add(:room_id, references(:rooms))

      timestamps()
    end

    create(index(:spirits, [:room_id]))
  end

  def down do
    drop(table(:spirits))
  end
end
