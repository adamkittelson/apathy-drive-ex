defmodule ApathyDrive.Repo.Migrations.CreateCharactersClasses do
  use Ecto.Migration

  def change do
    create table(:characters_classes) do
      add(:class_id, references(:classes, on_delete: :delete_all))
      add(:character_id, references(:characters, on_delete: :delete_all))
      add(:experience, :bigint)
      add(:level, :integer)
    end

    alter table(:characters) do
      remove(:level)
    end
  end
end
