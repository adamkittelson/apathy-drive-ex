defmodule ApathyDrive.Repo.Migrations.CreateCharactersRaces do
  use Ecto.Migration

  def change do
    create table(:characters_races) do
      add(:race_id, references(:races, on_delete: :delete_all))
      add(:character_id, references(:characters, on_delete: :delete_all))
      add(:strength_experience, :bigint, default: 0)
      add(:agility_experience, :bigint, default: 0)
      add(:intellect_experience, :bigint, default: 0)
      add(:willpower_experience, :bigint, default: 0)
      add(:health_experience, :bigint, default: 0)
      add(:charm_experience, :bigint, default: 0)
    end
  end
end
