defmodule ApathyDrive.Repo.Migrations.CreateCharactersItems do
  use Ecto.Migration

  def change do
    create table(:characters_items) do
      add(:character_id, references(:characters))
      add(:item_id, references(:items))
      add(:equipped, :boolean)
      add(:level, :integer)

      timestamps()
    end
  end
end
