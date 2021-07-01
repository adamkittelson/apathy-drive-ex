defmodule ApathyDrive.Repo.Migrations.AddCharacterAttunements do
  use Ecto.Migration

  def change do
    create table(:characters_attunements) do
      add(:character_id, references(:characters, on_delete: :delete_all))
      add(:obelisk_id, references(:items_instances, on_delete: :delete_all))
    end
  end
end
