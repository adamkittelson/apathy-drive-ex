defmodule ApathyDrive.Repo.Migrations.CreateCharacters do
  use Ecto.Migration

  def change do
    create table(:characters) do
      add :room_id, :integer
      add :class_id, :integer
      add :race_id, :integer
      add :name, :text
      add :gender, :text
      add :email, :text
      add :password, :text
      add :external_id, :text
      add :experience, :bigint, default: 0
      add :level, :integer, default: 1
      add :admin, :boolean
      add :inventory, :jsonb
      add :equipment, :jsonb
      add :flags, :jsonb

      timestamps
    end
    
    create index(:characters, ["lower(name)"], unique: true, name: :characters_lower_name_index)
    create index(:characters, ["lower(email)"], unique: true, name: :characters_lower_email_index)
  end
end
