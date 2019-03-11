defmodule ApathyDrive.Repo.Migrations.AddTrainerIdToRooms do
  use Ecto.Migration

  def change do
    alter table(:rooms) do
      add(:trainer_id, references(:trainers, on_delete: :nilify_all))
    end

    alter table(:trainers) do
      remove(:room_id)
    end
  end
end
