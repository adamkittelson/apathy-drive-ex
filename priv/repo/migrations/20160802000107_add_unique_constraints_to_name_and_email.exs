defmodule ApathyDrive.Repo.Migrations.AddUniqueConstraintsToNameAndEmail do
  use Ecto.Migration

  def change do
    create(index(:spirits, ["lower(name)"], unique: true, name: :spirits_lower_name_index))
    create(index(:spirits, ["lower(email)"], unique: true, name: :spirits_lower_email_index))
  end
end
