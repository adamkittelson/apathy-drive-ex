defmodule ApathyDrive.Repo.Migrations.CreateClassesSpellcastingAttributes do
  use Ecto.Migration

  def change do
    create table(:classes_spellcasting_attributes) do
      add(:class_id, references(:classes, on_delete: :delete_all))
      add(:attribute_id, references(:attributes, on_delete: :delete_all))
    end

    drop(table(:abilities_attributes))
  end
end
