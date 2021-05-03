defmodule ApathyDrive.Repo.Migrations.AddBlockChanceToItems do
  use Ecto.Migration

  def change do
    alter table(:items) do
      add(:block_chance, :integer)
    end
  end
end
