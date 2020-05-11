defmodule ApathyDrive.Repo.Migrations.CreateUniqueClassAbilityIndex do
  use Ecto.Migration

  def change do
    create(unique_index(:classes_abilities, [:class_id, :ability_id]))
  end
end
