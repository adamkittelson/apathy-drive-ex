defmodule ApathyDrive.Repo.Migrations.CreateCharacter do
  use Ecto.Migration

  def change do
    create table(:characters) do
      add(:name, :text)
      add(:player_id, :integer)
      add(:race_id, :integer)
      add(:class_id, :integer)
      add(:experience, :integer)
      add(:alignment, :integer)

      timestamps()
    end
  end
end
