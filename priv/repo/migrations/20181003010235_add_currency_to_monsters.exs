defmodule ApathyDrive.Repo.Migrations.AddCurrencyToMonsters do
  use Ecto.Migration

  def change do
    alter table(:monsters) do
      add(:copper, :integer)
      add(:silver, :integer)
      add(:gold, :integer)
      add(:platinum, :integer)
      add(:runic, :integer)
    end
  end
end
