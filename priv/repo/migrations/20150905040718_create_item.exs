defmodule ApathyDrive.Repo.Migrations.CreateItem do
  use Ecto.Migration

  def change do
    create table(:items) do
      add :name, :text
      add :description, :text
      add :weight, :integer
      add :worn_on, :text
      add :physical_defense, :integer
      add :magical_defense, :integer
      add :level, :integer
      add :strength, :integer
      add :agility, :integer
      add :will, :integer
      add :grade, :integer

      timestamps
    end

  end
end
