defmodule ApathyDrive.Repo.Migrations.AddCurrenciesToRooms do
  use Ecto.Migration

  def change do
    alter table(:rooms) do
      add(:copper, :integer, default: 0)
      add(:silver, :integer, default: 0)
      add(:gold, :integer, default: 0)
      add(:platinum, :integer, default: 0)
      add(:runic, :integer, default: 0)
    end
  end
end
