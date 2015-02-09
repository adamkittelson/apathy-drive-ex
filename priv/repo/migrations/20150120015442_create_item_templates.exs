defmodule ApathyDrive.Repo.Migrations.CreateItemTemplates do
  use Ecto.Migration

  def up do
    create table(:item_templates) do
      add :name,                  :text
      add :keywords,              {:array, :string}
      add :description,           :text
      add :slot,                  :text
      add :worn_on,               :jsonb
      add :hit_verbs,             {:array, :string}
      add :damage,                :jsonb
      add :required_skills,       :jsonb
      add :speed,                 :decimal
      add :accuracy_skill,        :text
      add :ac,                    :integer
      add :uses,                  :integer
      add :destruct_message,      :text
      add :room_destruct_message, :text
      add :can_pick_up,           :boolean
      add :cost,                  :integer
      add :light,                 :integer
      add :light_duration,        :integer
      add :always_lit,            :boolean

      timestamps
    end
  end

  def down do
    drop table(:item_templates)
  end
end
