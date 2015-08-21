defmodule ApathyDrive.Repo.Migrations.ChangeMonsterTemplateAbilitiesToJsonb do
  use Ecto.Migration

  def change do
    alter table(:monster_templates) do
      remove :abilities
      add :abilities, :jsonb
    end
  end
end
