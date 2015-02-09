defmodule ApathyDrive.Repo.Migrations.CreateMonsterTemplates do
  use Ecto.Migration

  def up do
    create table(:monster_templates) do
      add :name,             :text
      add :description,      :text
      add :death_message,    :text
      add :enter_message,    :text
      add :exit_message,     :text
      add :abilities,        {:array, :integer}
      add :greeting,         :text
      add :gender,           :text
      add :game_limit,       :integer
      add :adjectives,       {:array, :string}
      add :strength,         :integer
      add :agility,          :integer
      add :intelligence,     :integer
      add :health,           :integer
      add :skills,           :jsonb
      add :hit_verbs,        {:array, :string}
      add :chance_to_follow, :integer
      add :damage,           :jsonb
      add :disposition,      :text
      add :alignment,        :text
      add :possession_level, :integer
      add :limbs,            :jsonb
      add :questions,        :jsonb

      timestamps
    end
  end

  def down do
    drop table(:monster_templates)
  end

end
