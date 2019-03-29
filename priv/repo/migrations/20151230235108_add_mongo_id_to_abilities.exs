defmodule ApathyDrive.Repo.Migrations.AddMongoIdToAbilities do
  use Ecto.Migration

  def change do
    alter table(:abilities) do
      add(:mongo_id, :text)
    end

    alter table(:monster_templates) do
      remove(:abilities)
      remove(:skills)
      remove(:attacks)
      remove(:effects)
      remove(:hp_regen)
      remove(:max_hp)
    end
  end
end
