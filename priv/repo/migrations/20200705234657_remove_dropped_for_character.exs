defmodule ApathyDrive.Repo.Migrations.RemoveDroppedForCharacter do
  use Ecto.Migration

  def change do
    alter table(:items_instances) do
      remove(:dropped_for_character_id)
    end
  end
end
