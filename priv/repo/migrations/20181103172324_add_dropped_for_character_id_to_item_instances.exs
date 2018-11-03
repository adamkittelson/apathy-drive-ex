defmodule ApathyDrive.Repo.Migrations.AddDroppedForCharacterIdToItemInstances do
  use Ecto.Migration

  def change do
    alter table(:items_instances) do
      add(:dropped_for_character_id, references(:characters, on_delete: :nilify_all))
    end
  end
end
