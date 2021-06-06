defmodule ApathyDrive.Repo.Migrations.AddSockets do
  use Ecto.Migration

  def change do
    create table(:sockets) do
      add(:item_id, references(:items_instances, on_delete: :delete_all))
      add(:socketed_item_id, references(:items_instances, on_delete: :delete_all))
      add(:number, :integer)
    end
  end
end
