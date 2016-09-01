defmodule ApathyDrive.Repo.Migrations.AddPoeAttributesToMonsterTemplates do
  use Ecto.Migration

  def up do
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

  def down do
    alter table(:monster_templates) do
      remove :might
      remove :constitution
      remove :dexterity
      remove :perception
      remove :intellect
      remove :resolve

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
