defmodule ApathyDrive.Repo.Migrations.CreateExits do
  use Ecto.Migration

  def change do
    create table(:exits) do
      add(:kind, :text)
    end

    create table(:rooms_exits) do
      add(:exit_id, references(:exits, on_delete: :delete_all))
      add(:room_id, references(:rooms, on_delete: :delete_all))
      add(:destination_id, references(:rooms, on_delete: :delete_all))
      add(:direction, :text)
      add(:data, :map)
    end
  end
end
