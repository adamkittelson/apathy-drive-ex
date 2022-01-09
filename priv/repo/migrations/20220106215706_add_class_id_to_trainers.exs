defmodule ApathyDrive.Repo.Migrations.AddClassIdToTrainers do
  use Ecto.Migration

  def change do
    alter table(:trainers) do
      add(:class_id, references(:classes, on_delete: :delete_all))
    end
  end
end
