defmodule ApathyDrive.Repo.Migrations.AddCritFieldsToAbilities do
  use Ecto.Migration

  def change do
    alter table(:abilities) do
      add(:crit_table_id, references(:damage_types, on_delete: :delete_all))
      add(:letter, :text)
    end
  end
end
