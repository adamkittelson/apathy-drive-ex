defmodule ApathyDrive.Repo.Migrations.AddCurrenciesToCharacters do
  use Ecto.Migration

  def change do
    alter table(:characters) do
      add(:copper, :integer)
      add(:silver, :integer)
      add(:platinum, :integer)
      add(:runic, :integer)
    end
  end
end
