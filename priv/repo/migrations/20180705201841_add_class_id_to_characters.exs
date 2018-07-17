defmodule ApathyDrive.Repo.Migrations.AddClassIdToCharacters do
  use Ecto.Migration

  def change do
    alter table(:characters) do
      add(:class_id, references(:classes))
    end
  end
end
