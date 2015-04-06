defmodule ApathyDrive.Repo.Migrations.MonsterFields do
  use Ecto.Migration

  def up do
    alter table(:monsters) do
      remove :experience
      remove :alignment
      remove :level
      remove :skills
      remove :limbs
    end
    
    alter table(:monster_templates) do
      remove :possession_level
      add :level, :integer
    end
  end

  def down do
    alter table(:monsters) do
      add :experience, :integer
      add :alignment,  :integer
      add :level,      :integer
      add :skills,     :jsonb
      add :limbs,      :jsonb
    end
    alter table(:monster_templates) do
      remove :level
      add :possession_level, :integer
    end
  end
end
