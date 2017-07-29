defmodule ApathyDrive.Repo.Migrations.CascadeCharactersItems do
  use Ecto.Migration

  def change do
    alter table(:characters_items) do
      remove :character_id
      add :character_id, references(:characters, on_delete: :delete_all)
    end
  end
end
