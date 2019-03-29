defmodule ApathyDrive.Repo.Migrations.AddLairIdToMonsters do
  use Ecto.Migration

  def up do
    alter table(:monsters) do
      add(:lair_id, :integer)
    end
  end

  def down do
    alter table(:monsters) do
      remove(:lair_id)
    end
  end
end
