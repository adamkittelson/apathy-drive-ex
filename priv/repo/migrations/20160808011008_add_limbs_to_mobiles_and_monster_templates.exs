defmodule ApathyDrive.Repo.Migrations.AddLimbsToMobilesAndMonsterTemplates do
  use Ecto.Migration

  def change do
    alter table(:monster_templates) do
      add :limbs, :jsonb
    end

    alter table(:mobiles) do
      add :limbs, :jsonb
      add :crippled_limbs, {:array, :string}
      add :missing_limbs, {:array, :string}
    end
  end
end
