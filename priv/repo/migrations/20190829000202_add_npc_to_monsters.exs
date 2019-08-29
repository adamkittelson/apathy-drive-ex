defmodule ApathyDrive.Repo.Migrations.AddNpcToMonsters do
  use Ecto.Migration

  def change do
    alter table(:monsters) do
      add(:npc, :boolean, default: false)
    end
  end
end
