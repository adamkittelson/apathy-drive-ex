defmodule ApathyDrive.Repo.Migrations.CreateRaces do
  use Ecto.Migration

  def change do
    create table(:races) do
      add(:name, :text)
      add(:description, :text)
      add(:strength, :integer)
      add(:agility, :integer)
      add(:intellect, :integer)
      add(:willpower, :integer)
      add(:health, :integer)
      add(:charm, :integer)

      timestamps()
    end
  end
end
