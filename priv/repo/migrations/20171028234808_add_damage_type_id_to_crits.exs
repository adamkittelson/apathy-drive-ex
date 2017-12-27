defmodule ApathyDrive.Repo.Migrations.AddDamageTypeIdToCrits do
  use Ecto.Migration

  def change do
    alter table(:crits) do
      add :damage_type_id, references(:damage_types, on_delete: :delete_all)
    end
  end
end
