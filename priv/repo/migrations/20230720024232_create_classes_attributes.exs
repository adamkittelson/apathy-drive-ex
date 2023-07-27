defmodule ApathyDrive.Repo.Migrations.CreateClassesAttributes do
  use Ecto.Migration

  def change do
    create table(:classes_attributes) do
      add(:class_id, references(:classes, on_delete: :delete_all))
      add(:attribute_id, references(:attributes, on_delete: :delete_all))
      add(:ratio, :integer)
    end
  end
end
