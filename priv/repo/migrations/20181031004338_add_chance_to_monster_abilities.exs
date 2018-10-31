defmodule ApathyDrive.Repo.Migrations.AddChanceToMonsterAbilities do
  use Ecto.Migration

  def change do
    alter table(:monsters_abilities) do
      remove(:value)
      add(:chance, :integer)
    end
  end
end
