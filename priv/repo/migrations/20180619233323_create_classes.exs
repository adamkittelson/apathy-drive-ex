defmodule ApathyDrive.Repo.Migrations.CreateClasses do
  use Ecto.Migration

  def change do
    create table(:classes) do
      add(:name, :text)
      add(:description, :text)

      timestamps()
    end
  end
end
