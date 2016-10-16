defmodule ApathyDrive.Repo.Migrations.AddLimbsToMonstersAndMonsterTemplates do
  use Ecto.Migration

  def change do
    alter table(:monster_templates) do
      add :limbs, :jsonb
    end

    alter table(:monsters) do
      add :limbs, :jsonb
      add :crippled_limbs, {:array, :string}
      add :missing_limbs, {:array, :string}
    end
  end
end
