defmodule ApathyDrive.Repo.Migrations.AddBaseHpToMonsters do
  use Ecto.Migration

  def change do
    alter table(:monsters) do
      add(:base_hp, :integer)
    end
  end
end
