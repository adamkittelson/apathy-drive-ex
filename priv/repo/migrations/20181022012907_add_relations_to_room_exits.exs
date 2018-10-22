defmodule ApathyDrive.Repo.Migrations.AddRelationsToRoomExits do
  use Ecto.Migration

  def change do
    alter table(:rooms_exits) do
      add(:item_id, references(:items, on_delete: :delete_all))
      add(:class_id, references(:classes, on_delete: :delete_all))
      add(:race_id, references(:races, on_delete: :delete_all))
      add(:pre_move_ability_id, references(:abilities, on_delete: :delete_all))
      add(:post_move_ability_id, references(:abilities, on_delete: :delete_all))
      add(:ability_id, references(:abilities, on_delete: :delete_all))
    end
  end
end
