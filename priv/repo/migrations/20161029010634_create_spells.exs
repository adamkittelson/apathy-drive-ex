defmodule ApathyDrive.Repo.Migrations.CreateSpells do
  use Ecto.Migration

  def change do
    rename table(:abilities), to: table(:old_abilities)

    create table(:spells) do
      add :name, :text
      add :targets, :text
      add :kind, :text
      add :mana, :integer
      add :command, :text
      add :description, :text
      add :user_message, :text
      add :target_message, :text
      add :spectator_message, :text
      add :duration_in_ms, :integer

      timestamps
    end

    create table(:abilities) do
      add :name, :text
      add :description, :text

      timestamps
    end

    create table(:spells_abilities) do
      add :spell_id, :integer
      add :ability_id, :integer
      add :value, :jsonb

      timestamps
    end

    create table(:classes_spells) do
      add :class_id, :integer
      add :spell_id, :integer
      add :level, :integer

      timestamps
    end

  end
end
