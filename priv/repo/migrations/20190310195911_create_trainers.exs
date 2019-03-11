defmodule ApathyDrive.Repo.Migrations.CreateTrainers do
  use Ecto.Migration

  def change do
    create table(:trainers) do
      add(:min_level, :integer)
      add(:max_level, :integer)
      add(:class_id, references(:classes, on_delete: :delete_all))
      add(:room_id, references(:rooms, on_delete: :delete_all))
      add(:cost_multiplier, :float)
    end
  end
end
