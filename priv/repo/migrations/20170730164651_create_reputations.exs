defmodule ApathyDrive.Repo.Migrations.CreateReputations do
  use Ecto.Migration

  def change do
    create table(:areas_allies) do
      add(:area_id, references(:areas, on_delete: :delete_all))
      add(:ally_id, references(:areas, on_delete: :delete_all))
    end

    create table(:areas_enemies) do
      add(:area_id, references(:areas, on_delete: :delete_all))
      add(:enemy_id, references(:areas, on_delete: :delete_all))
    end

    create table(:characters_reputations) do
      add(:character_id, references(:characters, on_delete: :delete_all))
      add(:area_id, references(:areas, on_delete: :delete_all))
      add(:reputation, :float, default: 0.0)
    end

    create(index(:areas_allies, [:area_id, :ally_id], unique: true))
    create(index(:areas_enemies, [:area_id, :enemy_id], unique: true))
    create(index(:characters_reputations, [:character_id, :area_id], unique: true))
  end
end
