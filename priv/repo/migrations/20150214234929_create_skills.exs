defmodule ApathyDrive.Repo.Migrations.CreateSkills do
  use Ecto.Migration

  def up do
    create table(:skills) do
      add :name,         :text
      add :level,        :integer
      add :cost,         :decimal
      add :universal,    :boolean
      add :strength,     :integer
      add :agility,      :integer
      add :intelligence, :integer
      add :health,       :integer
      add :description,  :text

      timestamps
    end
  end

  def down do
    drop table(:skills)
  end
end
