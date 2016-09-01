defmodule ApathyDrive.Repo.Migrations.AddPoeAttributesToMonsterTemplates do
  use Ecto.Migration

  def change do
    alter table(:monster_templates) do
      add :might, :integer, default: 0
      add :constitution, :integer, default: 0
      add :dexterity, :integer, default: 0
      add :perception, :integer, default: 0
      add :intellect, :integer, default: 0
      add :resolve, :integer, default: 0

      add :dr, :integer
      add :slash_dr, :integer
      add :pierce_dr, :integer
      add :crush_dr, :integer
      add :shock_dr, :integer
      add :burn_dr, :integer
      add :freeze_dr, :integer
      add :corrode_dr, :integer

      add :max_hp, :integer
      add :hp_regen, :integer

      add :abilities, :jsonb

      remove :permanent
    end
  end
end
