defmodule ApathyDrive.Repo.Migrations.AddClassesTraits do
  use Ecto.Migration

  def change do
    create table(:classes_traits) do
      add(:class_id, references(:classes, on_delete: :delete_all))
      add(:trait_id, references(:traits, on_delete: :delete_all))
      add(:value, :jsonb)
    end

    create(index(:classes_traits, [:class_id, :trait_id], unique: true))
  end
end
