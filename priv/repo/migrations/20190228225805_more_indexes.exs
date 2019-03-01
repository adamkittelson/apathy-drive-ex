defmodule ApathyDrive.Repo.Migrations.MoreIndexes do
  use Ecto.Migration

  def change do
    create(index(:abilities, [:name]))
    create(index(:classes_abilities, [:class_id, :level]))
  end
end
