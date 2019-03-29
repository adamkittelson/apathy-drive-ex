defmodule ApathyDrive.Repo.Migrations.UpdateItems do
  use Ecto.Migration

  def change do
    alter table(:items) do
      add(:game_limit, :integer)
      add(:rarity, :text)
      add(:strength, :integer)
      add(:agility, :integer)
      add(:intellect, :integer)
      add(:willpower, :integer)
      add(:health, :integer)
      add(:charm, :integer)
      remove(:weight)
      remove(:level)
    end
  end
end
