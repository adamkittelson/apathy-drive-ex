defmodule ApathyDrive.Repo.Migrations.AlignmentFieldChanges do
  use Ecto.Migration

  def change do
    alter table(:characters) do
      remove(:evil_points)
      add(:bounty, :integer)
      add(:alignment, :string)
    end

    alter table(:monsters) do
      add(:lawful, :boolean, default: false)
      remove(:disposition)
    end

    drop(table(:characters_reputations))
    drop(table(:areas_enemies))
    drop(table(:areas_allies))
  end
end
