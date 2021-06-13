defmodule ApathyDrive.Repo.Migrations.AddSocketableToItems do
  use Ecto.Migration

  def change do
    alter table(:items) do
      add(:socketable, :boolean, default: false)
    end
  end
end
