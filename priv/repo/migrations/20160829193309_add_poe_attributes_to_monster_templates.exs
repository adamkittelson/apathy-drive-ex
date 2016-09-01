defmodule ApathyDrive.Repo.Migrations.AddPoeAttributesToMonsterTemplates do
  use Ecto.Migration

  def up do
    alter table(:monster_templates) do
      add :accuracy, :integer, default: 25

      add :fortitude, :integer, default: 20
      add :reflex, :integer, default: 20
      add :deflection, :integer, default: 20
      add :will, :integer, default: 20

      add :concentration, :integer, default: 50

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

  def down do
    alter table(:monster_templates) do
      remove :accuracy

      remove :fortitude
      remove :reflex
      remove :deflection
      remove :will

      remove :concentration

      remove :dr
      remove :slash_dr
      remove :pierce_dr
      remove :crush_dr
      remove :shock_dr
      remove :burn_dr
      remove :freeze_dr
      remove :corrode_dr

      remove :max_hp
      remove :hp_regen

      remove :abilities

      add :permanent, :boolean
    end
  end
end
